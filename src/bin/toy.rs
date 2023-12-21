/*
                                 Apache License
                           Version 2.0, January 2004
                        http://www.apache.org/licenses/

   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION
        copyright 2018 cranelift developers

        Modified by: Gama Sibusiso Vincent

*/

use core::mem;
use std::fs;
use clap::Parser;
use cranelift_jit_demo::{config::{self, Config}, jit}; 


fn main() -> Result<(), String> {
    let conf = Config::parse(); 

    if conf.file == "".to_string() {
        conf.clone().report(format!("no input file provided"));
    }


    // Create the JIT instance, which manages all generated functions and data.
    let mut jit = jit::JIT::default();
    let result = compile_and_run(&mut jit, conf.clone()); 

    if conf.run {
        match result {
            Ok(x) => {
                println!("\nprogram exited successfully with exit code {}", x);
            }
            Err(e) => {
                println!("err: {}", e);
            }
        }
    }
    Ok(())
}

fn compile_and_run(jit: &mut jit::JIT, config: Config) -> Result<isize, String> {
    let file_name = config.file.clone();

    let input = fs::read_to_string(file_name.as_str()); 
    let input = match input {
        Ok(i) => i, 
        Err(e) => {
            println!(
                "cannot compile file with name `{}` because of system reason: {}", 
                file_name, 
                e
            );
            std::process::exit(1); 
            unreachable!()
        }
    }; 


    unsafe { run_code(jit, input.as_str(), config) }
}


fn run_hello(jit: &mut jit::JIT, config: Config) -> Result<isize, String> {
    jit.create_data("hello_string", "hello world!\0".as_bytes().to_vec());
    unsafe { run_code(jit, HELLO_CODE, config) }
}

/// Executes the given code using the cranelift JIT compiler.
///
/// Feeds the given input into the JIT compiled function and returns the resulting output.
///
/// # Safety
///
/// This function is unsafe since it relies on the caller to provide it with the correct
/// input and output types. Using incorrect types at this point may corrupt the program's state.
unsafe fn run_code<O>(jit: &mut jit::JIT, code: &str, config: Config) -> Result<O, String> {
    // Pass the string to the JIT, and it returns a raw pointer to machine code.
    let code_ptr = jit.compile(code, config.clone());
    let code = match code_ptr {
        Err(err) => {
            if err == "done" {
                println!("Compiled successfully"); 
            } else {
                println!("{}", err);
            }

            std::process::exit(1); 
        }
        Ok(x) => {
            x
        }
    }; 
     
    // Cast the raw pointer to a typed function pointer. This is unsafe, because
    // this is the critical point where you have to trust that the generated code
    // is safe to be called.
    let code_fn = mem::transmute::<_, fn() -> O>(code);
    // And now we can call 
    if config.run {
        let result = code_fn();
        Ok(result)
    } else {
        Err("done".to_string()) 
    }
}

// Let's say hello, by calling into libc. The puts function is resolved by
/// dlsym to the libc function, and the string &hello_string is defined below.
const HELLO_CODE: &str = r#"
fn hello() -> (r) = {
    puts(&hello_string)
}
"#;
