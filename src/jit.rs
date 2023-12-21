/*                           Version 2.0, January 2004
                        http://www.apache.org/licenses/

   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION
        copyright 2018 cranelift developers

        Modified by: Gama Sibusiso Vincent

*/


use crate::config::Config;
use crate::errors::Error;
use cranelift::codegen::isa::CallConv;
use cranelift_module::FuncId;
use cranelift_module::FuncOrDataId;
use crate::frontend::*;
use crate::gentype::*; 
use cranelift::codegen::ir::types::I16;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};
use cranelift_native::builder;
use std::collections::HashMap;
use std::fs;
use std::slice;

/// The basic JIT class.
pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    data_description: DataDescription,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,

    /// Variable declaration index
    pub index: usize
}

impl Default for JIT {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
            index: 0, 
        }
    }
}


impl JIT {

    pub fn new() -> Self {
        JIT::default()
    }

    pub fn index(&mut self) -> usize {
        let i = self.index;
        self.index += 1; 
        i
    }

    pub fn prev_index(&mut self) -> usize {
        let i = self.index(); 
        if i == 0 {
            i 
        } else {
            i - 1
        }
    }
    pub fn type_to_ir_type(t: &str) -> Type {
        match t {
            "i32" => types::I32, 
            "i16" => types::I16, 
            "i64" => types::I64, 
            "i8" => types::I8,
            "bool" => types::I8X2, 
            "f32" => types::F32, 
            "f64" => types::F64, 
            _ => unreachable!()
        }
    }

    pub fn type_as_string(t: Type) -> String {
        match t {
            types::I32 => "i32".to_string(), 
            types::I16 => "i16".to_string(), 
            types::I8  => "i8".to_string(), 
            types::I64 => "i64".to_string(), 
            types::I8X2 => "bool".to_string(),
            types::F32 => "f32".to_string(), 
            types::F64 => "f64".to_string(), 
            _ => todo!()
        }
    }

    pub fn type_repr_to_ir(t: Type) -> Type {
        match t {
            types::I8X2 => types::I8,
            _ => t
        }
    }

    pub fn get_type_mapping_for_name(name: String, bindings: Vec<(String, String)>) -> Option<Type> {
        for (index, bind) in bindings.into_iter().enumerate() {
            let (_name, _type) = bind; 
            if _name == name {
                return Some(JIT::type_to_ir_type(_type.as_str()));
            }
        }
        None
    }

    /// Compile a string in the toy language into machine code.
    pub fn compile(&mut self, input: &str, config: Config) -> Result<*const u8, String> {
        // First, parse the string, producing AST nodes.
        let statements: Vec<Statement> = parser::top_levels(input).map_err(| e| e.to_string())?; 
        for statement in statements { 

            match statement {
                Statement::Function(name, params, the_return, body, bindings) => {
                    let code = self.translate_function(name.clone(), params, the_return, body, bindings, config.clone())?; 
                    if name == "main".to_string() {
                        return Ok(code); 
                    }
                }
                Statement::Import(name) => {
                    // let mut jit = JIT::default(); 
                    let _ = self.translate_import(name.clone(), config.clone());
                    //println!("import {} -> {:?}", name, code);
                }
                _ => {
                    continue;
                }
            }; 
        }

        Err("done".to_string())
    }

    fn translate_import(&mut self, tree: Vec<String>, config: Config) -> Result<*const u8, String> {
        let tree_ = tree.clone();
        let name_ = tree_.first().unwrap(); 
        let mut file_name = name_.clone(); 
        file_name.push_str(".zulu"); 

        let mut imports = config.import_paths.clone(); 
        let mut import_paths = vec!["./".to_string(), "./lib/".to_string(), /*TODO: install folder*/];
        import_paths.append(&mut imports); 
        let paths = import_paths.join(" ").to_string(); 

        let mut input = "".to_string(); 
        for path in import_paths {
            let mut file_path = path.to_string(); 
            file_path.push_str(file_name.clone().as_str()); 

            let res = fs::read_to_string(file_path.as_str()); 
            match res {
                Ok(i) => {
                    input = i;
                }
                _ => {
                    continue;
                }
            }
        }

        if input == "".to_string() {
            Error::report(format!(
                "cannot import file with name {} from path list [{}] because of reason: {}", 
                name_, 
                paths, 
                "File not found!"
            ));
        }
        
        let tree_len = tree_.len();
         let tree_rest = tree_.get(1 .. tree_len).unwrap(); 
        let tree_rest = tree_rest.to_vec();
        let tree_len = tree_rest.len();

        if tree_len == 1 {
            let last = tree_rest.last().unwrap(); 
            if last == "*" {
                let code = self.compile(input.as_str(), config); 
                return code;
            }
        }

        // First, parse the string, producing AST nodes.
        let statements: Vec<Statement> = parser::top_levels(input.as_str()).map_err(| e| e.to_string())?; 

        let mut check_for_func = |name: String, i: i32| -> Result<*const u8, String> {
            for (index, statement) in statements.clone().into_iter().enumerate() {
                let func = tree_rest.clone().get(i as usize).unwrap().clone(); 
                match statement {
                    Statement::Function(name, params, the_return, body, bindings) => {
                        if name == func.clone() {
                            let code = self.translate_function(name.clone(), params, the_return, body, bindings, config.clone())?; 
                            return Ok(code);
                        }
                    }
                    Statement::Import(names) => {
                        self.translate_import(names.clone(), config.clone()); 
                    }
                    _ => todo!()
                }; 
            }

            Err("not found".to_string())
        };


        let code = {
            for (index_, func_name) in tree_rest.clone().into_iter().enumerate() {
                let code = check_for_func(func_name.clone(), index_ as i32); 
            }

            let func = tree_rest.last().unwrap(); 
            Err(format!("failed to import function {} from file {}", func, name_))
        };

        /*for decl in self.module.declarations().get_functions() {
            println!("\n{:?}\n", decl);
        }*/

        code
    }

    fn translate_function(
        &mut self, 
        name: String, 
        params: Vec<String>, 
        the_return: String, 
        stmts: Expr, 
        bindings: Expr, 
        config: Config
    ) -> Result<*const u8, String> {
        
        if config.defs {
            print!("{}", name);
        }
        // Then, translate the AST nodes into Cranelift IR.
        let id = self.translate(name.clone(), params, the_return, stmts, bindings, config)?;




        // Define the function to jit. This finishes compilation, although
        // there may be outstanding relocations to perform. Currently, jit
        // cannot finish relocations until all functions to be called are
        // defined. For this toy demo for now, we'll just finalize the
        // function below.
        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| e.to_string())?;


        // Next, declare the function to jit. Functions must be declared
        // before they can be called, or defined.
        //
        // TODO: This may be an area where the API should be streamlined; should
        // we have a version of `declare_function` that automatically declares
        // the function?
       // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        self.module.finalize_definitions().unwrap();

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(id);

        Ok(code)
    }

    /// Create a zero-initialized data section.
    pub fn create_data(&mut self, name: &str, contents: Vec<u8>) -> Result<&[u8], String> {
        // The steps here are analogous to `compile`, except that data is much
        // simpler than functions.
        self.data_description.define(contents.into_boxed_slice());
        let id = self
            .module
            .declare_data(name, Linkage::Export, true, false)
            .map_err(|e| e.to_string())?;

        self.module
            .define_data(id, &self.data_description)
            .map_err(|e| e.to_string())?;
        self.data_description.clear();
        self.module.finalize_definitions().unwrap();
        let buffer = self.module.get_finalized_data(id);
        // TODO: Can we move the unsafe into cranelift?
        Ok(unsafe { slice::from_raw_parts(buffer.0, buffer.1) })
    }

    // Translate from toy-language AST nodes into Cranelift IR.
    fn translate(
        &mut self,
        name: String, 
        params: Vec<String>,
        the_return: String,
        stmts: Expr,
        bindings: Expr,
        config: Config
    ) -> Result<FuncId, String> {
        // Our toy language currently only supports I64 values, though Cranelift
        // supports other types.
        let int = self.module.target_config().pointer_type();

        let binds = match bindings.clone() {
            Expr::Where(binds) => binds, 
            _ => panic!("Expected a where clause")
        }; 

        for _p in params.clone().into_iter() {
            let ty = JIT::get_type_mapping_for_name(_p.clone(), binds.clone()).unwrap_or(types::I32); 
            let ty = JIT::type_repr_to_ir(ty); 

            self.ctx.func.signature.params.push(AbiParam::new(ty));
        }

        let ty = JIT::get_type_mapping_for_name(the_return.clone(), binds.clone()).unwrap_or(int); 
        let ty = JIT::type_repr_to_ir(ty); 
        // Our toy language currently only supports one return value, though
        // Cranelift is designed to support more.
        self.ctx.func.signature.returns.push(AbiParam::new(ty));

        self.ctx.func.signature.call_conv = CallConv::Fast; 

        // Create the builder to build a function.
        let func = self.ctx.func.clone();

        let sig = func.signature.clone(); 

         let id = self
            .module
            .declare_function(&name, Linkage::Export, &sig)
            .map_err(|e| e.to_string())?;

        let mut builder = FunctionBuilder::new(&mut self.ctx.func , &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();

        // Since this is the entry block, add block parameters corresponding to
        // the function's parameters.
        //
        // TODO: Streamline the API here.
        builder.append_block_params_for_function_params(entry_block);

        // Tell the builder to emit code in this block.
        builder.switch_to_block(entry_block);

        // And, tell the builder that this block will have no further
        // predecessors. Since it's the entry block, it won't have any
        // predecessors.
        builder.seal_block(entry_block);

        // The toy language allows variables to be declared implicitly.
        // Walk the AST and declare all implicitly-declared variables.
        let index = 0;  
        let mut variables =
            declare_variables(int, &mut builder, &params, &the_return, stmts.clone(), binds.clone(), index,  entry_block, config.clone());

        //let ty = JIT::get_type_mapping_for_name("bool".to_string(), binds.clone()).unwrap_or(types::I8); 
        let ty = JIT::type_repr_to_ir(JIT::type_to_ir_type("bool"));  
        let mut index = index + 10000; 

        let t_variable = declare_variable(ty, &mut builder, &mut variables, &mut index, "true");
        
        let f_variable = declare_variable(ty, &mut builder, &mut variables, &mut index, "false");

        let t_val = builder.ins().iconst(ty, 1);  
        let f_val = builder.ins().iconst(ty, 0);  

        builder.def_var(t_variable.crane, t_val);
        builder.def_var(f_variable.crane, f_val);

        declare_variables_in_stmt(int, &mut builder, &mut variables, &mut index, &stmts);
        declare_variables_in_stmt(int, &mut builder, &mut variables, &mut index, &stmts);

        
        // println!("{:?}", variables); 
        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            int,
            builder,
            variables,
            module: &mut self.module,
            var_index: index as i32,
            current: name.clone()
        };
       
        trans.translate_expr(stmts.clone());

        // Set up the return variable of the function. Above, we declared a
        // variable to hold the return value. Here, we just do a use of that
        // variable.
        let return_variable = trans.variables.get(&the_return).unwrap();
        let return_value = trans.builder.use_var(return_variable.crane);


        // Emit the return instruction.
        trans.builder.ins().return_(&[return_value]);

        if config.ir {
            println!("{}", trans.builder.func.display()); 
        }
        // Tell the builder we're done with this function.
        trans.builder.finalize();
        Ok(id)
    }
}


/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
struct FunctionTranslator<'a> {
    int: types::Type,
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Var>,
    module: &'a mut JITModule,
    var_index: i32, 
    current: String
}

impl<'a> FunctionTranslator<'a> {
    /// When you write out instructions in Cranelift, you get back `Value`s. You
    /// can then use these references in other instructions.
    ///
    fn new_index(&mut self) -> i32 {
        let s = self.var_index; 
        self.var_index += 1; 
        s
    }


    fn gen_expr_short(&mut self, target: Expr, value: Expr) -> Val {

        let mut name = "".to_string(); 
        let is_id = match target {
            Expr::Identifier(s) => {
                name = s.clone(); 
                true
            }
            _ => false
        }; 

        if is_id == false {
            Error::report(format!(
                "Expected a variable on the lhs of the short hand assignment operator"
            ));
        }

        let e = Expr::Assign(name, Box::new(value)); 
        let crane = self.translate_expr(e); 
        crane
    }


    fn translate_expr(&mut self, expr: Expr) -> Val {
        match expr {
            Expr::SizeOf(e) => {
                let lhs = self.translate_expr(*e);
                let ty = lhs.datatype.bits(); 
                let new_expr = Expr::IntLiteral(ty.to_string()); 
                let lhs = self.translate_expr(new_expr); 
                Val::new(lhs.crane, lhs.datatype)
            }

            Expr::Neg(e) => {
                let lhs = self.translate_expr(*e); 


                if integer_type(lhs.datatype) == false {
                    Error::report(format!("invalid use of operator `-` (negation) with type {:?}", lhs.datatype));  
                }

                let crane = self.builder.ins().ineg(lhs.crane); 
                Val::new(crane, lhs.datatype)
            }

            Expr::BitNot(e) => {
                let lhs = self.translate_expr(*e);
                let crane = self.builder.ins().bnot(lhs.crane); 
                Val::new(crane, lhs.datatype)
            }

            Expr::Shl(x, y) => {
                let lhs = self.translate_expr(*x); 
                let rhs = self.translate_expr(*y); 


                if integer_type(lhs.datatype) == false && integer_type(rhs.datatype) == false {
                    if lhs.datatype != rhs.datatype {
                        Error::report(format!("invalid use of operator `+` on types {:?} and {:?}. `+` only works with integer types", lhs.datatype, rhs.datatype)); 
                    } else {
                        if integer_type(lhs.datatype) == false {
                            Error::report(format!("invalid use of operator `+` with type {:?}", lhs.datatype));  
                        }
                    }
                }

                let crane = self.builder.ins().ishl(lhs.crane, rhs.crane); 
                Val::new(crane, lhs.datatype)
            }

            Expr::Shr(x, y) => {
                let lhs = self.translate_expr(*x); 
                let rhs = self.translate_expr(*y); 


                if integer_type(lhs.datatype) == false && integer_type(rhs.datatype) == false {
                    if lhs.datatype != rhs.datatype {
                        Error::report(format!("invalid use of operator `+` on types {:?} and {:?}. `+` only works with integer types", lhs.datatype, rhs.datatype)); 
                    } else {
                        if integer_type(lhs.datatype) == false {
                            Error::report(format!("invalid use of operator `+` with type {:?}", lhs.datatype));  
                        }
                    }
                }
                let crane = self.builder.ins().ushr(lhs.crane, rhs.crane); 
                Val::new(crane, lhs.datatype)
            }



            Expr::BitXor(x, y) => {
                let lhs = self.translate_expr(*x); 
                let rhs = self.translate_expr(*y); 


                if integer_type(lhs.datatype) == false && integer_type(rhs.datatype) == false {
                    if lhs.datatype != rhs.datatype {
                        Error::report(format!("invalid use of operator `+` on types {:?} and {:?}. `+` only works with integer types", lhs.datatype, rhs.datatype)); 
                    } else {
                        if integer_type(lhs.datatype) == false {
                            Error::report(format!("invalid use of operator `+` with type {:?}", lhs.datatype));  
                        }
                    }
                }
                let crane = self.builder.ins().bxor(lhs.crane, rhs.crane); 
                Val::new(crane, lhs.datatype)
            }

            Expr::BitAnd(x, y) => {
                let lhs = self.translate_expr(*x); 
                let rhs = self.translate_expr(*y); 


                if integer_type(lhs.datatype) == false && integer_type(rhs.datatype) == false {
                    if lhs.datatype != rhs.datatype {
                        Error::report(format!("invalid use of operator `+` on types {:?} and {:?}. `+` only works with integer types", lhs.datatype, rhs.datatype)); 
                    } else {
                        if integer_type(lhs.datatype) == false {
                            Error::report(format!("invalid use of operator `+` with type {:?}", lhs.datatype));  
                        }
                    }
                }


                let crane = self.builder.ins().band(lhs.crane, rhs.crane); 
                Val::new(crane, lhs.datatype)
            }
            Expr::BitOr(x, y) => {
                let lhs = self.translate_expr(*x); 
                let rhs = self.translate_expr(*y); 


                if integer_type(lhs.datatype) == false && integer_type(rhs.datatype) == false {
                    if lhs.datatype != rhs.datatype {
                        Error::report(format!("invalid use of operator `+` on types {:?} and {:?}. `+` only works with integer types", lhs.datatype, rhs.datatype)); 
                    } else {
                        if integer_type(lhs.datatype) == false {
                            Error::report(format!("invalid use of operator `+` with type {:?}", lhs.datatype));  
                        }
                    }
                }


                let crane = self.builder.ins().bor(lhs.crane, rhs.crane); 
                Val::new(crane, lhs.datatype)
            }


            Expr::IncBy(target, value) => self.gen_expr_short(*target.clone(), Expr::Add(target, value)), 
            Expr::DecBy(target, value) => self.gen_expr_short(*target.clone(), Expr::Sub(target, value)), 
            Expr::MulBy(target, value) => self.gen_expr_short(*target.clone(), Expr::Mul(target, value)), 
            Expr::DivBy(target, value) => self.gen_expr_short(*target.clone(), Expr::Div(target, value)), 
            Expr::Unary(_main, _alt) => {
                let alt = self.translate_expr(*_alt); 
                let main = self.translate_expr(*_main);

                let main_type = main.datatype; 
                let alt_type = alt.datatype; 

                if main_type != alt_type {
                    Error::report(format!(
                      "invalid use of operator `??`. expected both types to be the same but found {} and {}", 
                        JIT::type_as_string(main_type), 
                        JIT::type_as_string(alt_type)
                    ));
                }

                let crane = self.builder.ins().select(main.crane, main.crane, alt.crane);
                Val::new(crane, main_type)
            }, 
            Expr::StringLiteral(_) => todo!(), 
            Expr::IntLiteral(literal) => {
                let imm: i32 = literal.parse().unwrap();
                let ty = JIT::type_to_ir_type("i32"); 
                let crane = self.builder.ins().iconst(ty, i64::from(imm)); 
                Val::new(crane, ty)
            }

            Expr::FloatLiteral(literal) => {
                let imm: f32 = literal.parse::<f32>().unwrap(); 
                let ty = JIT::type_to_ir_type("f32"); 
                let crane = self.builder.ins().f32const(imm); 
                Val::new(crane, ty)
            }


            Expr::Add(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);
                
                if integer_type(lhs.datatype) == false && integer_type(rhs.datatype) == false {
                    if lhs.datatype != rhs.datatype {
                        Error::report(format!("invalid use of operator `+` on types {:?} and {:?}. `+` only works with integer types", lhs.datatype, rhs.datatype)); 
                    } else {
                        if integer_type(lhs.datatype) == false {
                            Error::report(format!("invalid use of operator `+` with type {:?}", lhs.datatype));  
                        }
                    }
                }

                let crane = self.builder.ins().iadd(lhs.crane, rhs.crane); 
                Val::new(crane, lhs.datatype)
            }

            Expr::Sub(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);

                if integer_type(lhs.datatype) == false && integer_type(rhs.datatype) == false {
                    if lhs.datatype != rhs.datatype {
                        Error::report(format!("invalid use of operator `-` on types {:?} and {:?}. `-` only works with integer types", lhs.datatype, rhs.datatype)); 
                    } else {
                        if integer_type(lhs.datatype) == false {
                            Error::report(format!("invalid use of operator `-` with type {:?}", lhs.datatype));  
                        }
                    }
                }

                let crane = self.builder.ins().isub(lhs.crane, rhs.crane);
                
                Val::new(crane, lhs.datatype)
            }

            Expr::Mul(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);


                if integer_type(lhs.datatype) == false && integer_type(rhs.datatype) == false {
                    if lhs.datatype != rhs.datatype {
                        Error::report(format!("invalid use of operator `*` on types {:?} and {:?}. `*` only works with integer types", lhs.datatype, rhs.datatype)); 
                    } else {
                        if integer_type(lhs.datatype) == false {
                            Error::report(format!("invalid use of operator `*` with type {:?}", lhs.datatype));  
                        }
                    }
                }

                let crane = self.builder.ins().imul(lhs.crane, rhs.crane); 
                
                Val::new(crane, lhs.datatype)
            }

            Expr::Div(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);

                if integer_type(lhs.datatype) == false && integer_type(rhs.datatype) == false {
                    if lhs.datatype != rhs.datatype {
                        Error::report(format!("invalid use of operator `/` on types {:?} and {:?}. `/` only works with integer types", lhs.datatype, rhs.datatype)); 
                    } else {
                        if integer_type(lhs.datatype) == false {
                            Error::report(format!("invalid use of operator `/` with type {:?}", lhs.datatype));  
                        }
                    }
                }

                let crane = self.builder.ins().udiv(lhs.crane, rhs.crane); 
                
                Val::new(crane, lhs.datatype)
            }


            Expr::Mod(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);

                if integer_type(lhs.datatype) == false && integer_type(rhs.datatype) == false {
                    if lhs.datatype != rhs.datatype {
                        Error::report(format!("invalid use of operator `/` on types {:?} and {:?}. `/` only works with integer types", lhs.datatype, rhs.datatype)); 
                    } else {
                        if integer_type(lhs.datatype) == false {
                            Error::report(format!("invalid use of operator `/` with type {:?}", lhs.datatype));  
                        }
                    }
                }

                let crane = self.builder.ins().urem(lhs.crane, rhs.crane); 
                
                Val::new(crane, lhs.datatype)
            }


            Expr::Eq(lhs, rhs) => self.translate_icmp(IntCC::Equal, *lhs, *rhs),
            Expr::Ne(lhs, rhs) => self.translate_icmp(IntCC::NotEqual, *lhs, *rhs),
            Expr::Lt(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThan, *lhs, *rhs),
            Expr::Le(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThanOrEqual, *lhs, *rhs),
            Expr::Gt(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThan, *lhs, *rhs),
            Expr::Ge(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThanOrEqual, *lhs, *rhs),
            Expr::Call(name, args) => self.translate_call(name, args),
            Expr::GlobalDataAddr(name) => self.translate_global_data_addr(name),
            Expr::Identifier(name) => {
                // `use_var` is used to read the value of a variable.
                let variable = self.variables.get(&name); 

                match variable {
                    None => {
                        Error::report(format!("variable {} is not defined", name)); 
                    }
                    _ => {}
                }

                let variable = variable.unwrap().clone(); 
                let crane = self.builder.use_var(variable.crane);
                Val::new(crane, JIT::type_repr_to_ir(variable.datatype))
            }
            Expr::Assign(name, expr) => self.translate_assign(name, *expr),
            Expr::IfElse(condition, then_body, else_body) => {
                self.translate_if_else(*condition, *then_body, *else_body)
            }
            Expr::WhileLoop(condition, loop_body) => {
                self.translate_while_loop(*condition, loop_body)
            }
            Expr::Where(_) => unreachable!(), 
            Expr::Block(v) => self.translate_block(v), 
            Expr::Let(name, t, exp) => self.translate_let(name, t, *exp),
            Expr::Match(expr, cases) => self.translate_match(expr, cases), 
        }
    }

    fn translate_block(&mut self, stmts: Vec<Expr>) -> Val {
        let len = stmts.len(); 
        
        if len == 0 {
            Error::report("invalid block with 0 expressions. note the last expression in a block gets returned to the upper block.".to_string()); 
        }

        for (i, stmt) in stmts.clone().into_iter().enumerate() {
            if i == len - 1 {
                break; 
            }
            self.translate_expr(stmt); 
        }


        let last = stmts.last().unwrap(); 
        let ret = self.translate_expr(last.clone()); 
        Val::new(ret.crane, ret.datatype)
    }

    fn translate_let(&mut self, name: String, ty: String, expr: Expr) -> Val {
        // `def_var` is used to write the value of a variable. Note that
        // variables can have multiple definitions. Cranelift will
        // convert them into SSA form for itself automatically.
        let new_value = self.translate_expr(expr);
        let variable = self.variables.get(&name); 
       
        let ty = if ty != "".to_string() {
            JIT::type_repr_to_ir(JIT::type_to_ir_type(ty.as_str()))
        } else {
            new_value.datatype
        };

    
        let variable = match variable {
            Some(v) => v.clone(), 
            None => {
                let mut index = self.new_index() as usize; 
                let var = declare_variable(
                    ty, 
                    &mut self.builder, 
                    &mut self.variables, 
                    &mut index, 
                    name.as_str()
                );
                var
            }
        }; 

        //builder.def_var(var.crane, val);

        if new_value.datatype != variable.datatype {
            Error::report(format!(
                "invalid assignment to lhs side variable {} of type {:?} with value of type {:?}", 
                name, 
                variable.datatype, 
                new_value.datatype
            )); 
        }

        self.builder.def_var(variable.crane, new_value.crane);

        Val::new(new_value.crane, ty)
    }



    fn translate_assign(&mut self, name: String, expr: Expr) -> Val {
        // `def_var` is used to write the value of a variable. Note that
        // variables can have multiple definitions. Cranelift will
        // convert them into SSA form for itself automatically.
        let new_value = self.translate_expr(expr);
        let variable = self.variables.get(&name); 

        match variable {
            None => {
                Error::report(format!("cannot assign to variable `{}` because it has not been declared", name)); 
            }
            _ => {}
        }

        let variable = variable.unwrap();

        if integer_type(new_value.datatype) == false && integer_type(variable.datatype) == false {
            if new_value.datatype != variable.datatype {
                Error::report(format!("invalid assignment to variable {} of type {:?} with value of type {:?}", name, variable.datatype, new_value.datatype)); 
            }
        }

        self.builder.def_var(variable.crane, new_value.crane);

        Val::new(new_value.crane, JIT::type_repr_to_ir(new_value.datatype))
    }

    fn translate_icmp(&mut self, cmp: IntCC, lhs: Expr, rhs: Expr) -> Val {
        let lhs = self.translate_expr(lhs);
        let rhs = self.translate_expr(rhs);


        if lhs.datatype == rhs.datatype && lhs.datatype == JIT::type_to_ir_type("f32") {

            let cmp_ = match cmp {
                IntCC::Equal => FloatCC::Equal, 
                IntCC::NotEqual => FloatCC::NotEqual, 
                IntCC::SignedLessThan => FloatCC::LessThan, 
                IntCC::SignedLessThanOrEqual => FloatCC::LessThanOrEqual, 
                IntCC::SignedGreaterThan => FloatCC::GreaterThan,
                IntCC::SignedGreaterThanOrEqual => FloatCC::GreaterThanOrEqual,
                _ => unreachable!()
            }; 
    
            let crane = self.builder.ins().fcmp(cmp_, lhs.crane, rhs.crane);

            let ty = JIT::type_to_ir_type("bool"); 
            return Val::new(crane, ty); 
        }
        
        if integer_type(lhs.datatype) == false && integer_type(rhs.datatype) == false {
            if lhs.datatype != rhs.datatype {
                Error::report(format!("invalid use of bool operator on types {:?} and {:?}. bool operators only works with integer types", lhs.datatype, rhs.datatype)); 
            } else {
                if integer_type(lhs.datatype) == false {
                    Error::report(format!("invalid use of bool operator with type {:?}", lhs.datatype));  
                }
            }
            unreachable!()
        }


        let crane = self.builder.ins().icmp(cmp, lhs.crane, rhs.crane);

        let ty = JIT::type_to_ir_type("bool"); 
        Val::new(crane, ty)
    }

    fn instance_of_type(&mut self, t: Type) -> Val {
        let val = if integer_type(t) {
            self.builder.ins().iconst(t, 0)
        } else {
            match t {
                types::F32 => self.builder.ins().f32const(0 as f32),
                types::F64 => self.builder.ins().f64const(0 as f64),
                types::I8X2 => self.builder.ins().iconst(types::I8, 0), 
                _ => unreachable!()
            }
        }; 

        Val::new(val, t)
    }


    fn translate_match(&mut self, condition: Box<Expr>, cases: Vec<(Box<Expr>, Box<Expr>)>) -> Val {
        let condition_value = self.translate_expr(*condition.clone());

        let (case, body) = cases.get(0).unwrap(); 
        let case = self.translate_expr(*case.clone()); 
        let crane = self.builder.ins().icmp(IntCC::Equal, case.crane, condition_value.crane);
 
        let end_block = self.builder.create_block(); 
        //self.builder.seal_block(end_block);
        //
        let true_block = self.builder.create_block(); 
        let false_block = self.builder.create_block(); 

        self.builder
            .ins()
            .brif(crane, true_block, &[], false_block, &[]);

        self.builder.switch_to_block(true_block);
        self.builder.seal_block(true_block);
        let body_ir = self.translate_expr(*body.clone());
        
        //self.builder.ins().return_(&[body_ir.crane]); 
        self.builder.append_block_param(end_block, body_ir.datatype); 
        self.builder.ins().jump(end_block, &[body_ir.crane]);


        let mut blocks = vec![false_block, self.builder.create_block()]; 

        let len = cases.len(); 
        let sliced = cases.get(1 .. len - 1).unwrap().to_vec(); 
        let rest_blocks: Vec<_> = sliced.into_iter().map(|_| {
            self.builder.create_block()
        }).collect(); 


        blocks.extend_from_slice(rest_blocks.as_slice());


        self.builder.switch_to_block(blocks.get(0).unwrap().clone());
        self.builder.seal_block(blocks.get(0).unwrap().clone());

        //self.builder.ins().jump(end_block, &[body_ir.crane]);
        
 
        for (index, pack) in cases.clone().into_iter().enumerate() {
            
            if index == 0 {
                continue; 
            }

            if index == len - 2 || index == len - 1 {
                break;
            }

            let (case, body) = pack; 
            let case = self.translate_expr(*case.clone()); 
            let crane = self.builder.ins().icmp(IntCC::Equal, case.crane, condition_value.crane);
     
            //self.builder.seal_block(end_block);
            
            let true_block = blocks.get(index).unwrap().clone(); 
            let false_block = blocks.get(index + 1).unwrap().clone(); 

            self.builder
                .ins()
                .brif(crane, true_block, &[], false_block, &[]);

            self.builder.switch_to_block(true_block);
            self.builder.seal_block(true_block);
            let body_ir = self.translate_expr(*body.clone());

            //self.builder.ins().return_(&[body_ir.crane]); 
            self.builder.ins().jump(end_block, &[body_ir.crane]);


            self.builder.switch_to_block(false_block);
            self.builder.seal_block(false_block);

        }

        // Jump to the merge block, passing it the block return value.


        //self.builder.append_block_params_for_function_params(entry); 
        //self.builder.seal_block(entry); 

        // last case
        let (case, body) = cases.get(len - 2).unwrap(); 
 
        let is_default = match *case.clone() {
            Expr::Identifier(s) => s.clone() == "_".to_string(), 
            _ => false
        }; 

        if is_default == false {
            let case = self.translate_expr(*case.clone()); 
            let crane = self.builder.ins().icmp(IntCC::Equal, case.crane, condition_value.crane);
     
            //self.builder.seal_block(end_block);
            
            let true_block = self.builder.create_block(); 
            let false_block = self.builder.create_block(); 

            // println!("{:?} <- {:?}", true_block, blocks);

            self.builder
                .ins()
                .brif(crane, true_block, &[], false_block, &[]);

            self.builder.switch_to_block(true_block);
            self.builder.seal_block(true_block);
            let body_ir = self.translate_expr(*body.clone());
            
            //self.builder.ins().return_(&[body_ir.crane]); 
            self.builder.ins().jump(end_block, &[body_ir.crane]);


            self.builder.switch_to_block(false_block);
            self.builder.seal_block(false_block);
        }


        let (last_case, last_body) = cases.get(len -1).unwrap(); 

        let is_default = match *last_case.clone() {
            Expr::Identifier(s) => s.clone() == "_".to_string(), 
            _ => false
        }; 

        if is_default == false {
            Error::report(format!(
                "Expected the last case to be the default case in match expression"
            ));
        }

        let default_body = self.translate_expr(*last_body.clone()); 
        self.builder.ins().jump(end_block, &[default_body.crane]);

        // Switch to the merge block for subsequent statements.
        self.builder.switch_to_block(end_block);

        // We've now seen all the predecessors of the merge block.
        self.builder.seal_block(end_block);


        let phi = self.builder.block_params(end_block)[0];
        Val::new(phi, body_ir.datatype)
    }


    fn translate_if_else(
        &mut self,
        condition: Expr,
        then_body: Expr,
        else_body: Expr,
    ) -> Val {
        let condition_value = self.translate_expr(condition);

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        // If-else constructs in the toy language have a return value.
        // In traditional SSA form, this would produce a PHI between
        // the then and else bodies. Cranelift uses block parameters,
        // so set up a parameter in the merge block, and we'll pass
        // the return values to it from the branches.

        // Test the if condition and conditionally branch.
        self.builder
            .ins()
            .brif(condition_value.crane, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let then_return = self.translate_expr(then_body);

        self.builder.append_block_param(merge_block, then_return.datatype);

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[then_return.crane]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        
        let else_return = self.translate_expr(else_body);

        if then_return.datatype != else_return.datatype {
            Error::report(format!(
                "invalid returns in if statement, the true block returns type {} and the false block returns type {}", 
                JIT::type_as_string(then_return.datatype), 
                JIT::type_as_string(else_return.datatype)
            )); 
        }
        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[else_return.crane]);

        // Switch to the merge block for subsequent statements.
        self.builder.switch_to_block(merge_block);

        // We've now seen all the predecessors of the merge block.
        self.builder.seal_block(merge_block);

        // Read the value of the if-else by reading the merge block
        // parameter.
        let phi = self.builder.block_params(merge_block)[0];

        Val::new(phi, else_return.datatype)
    }

    fn translate_while_loop(&mut self, condition: Expr, loop_body: Box<Expr>) -> Val {
        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let condition_value = self.translate_expr(condition);
        self.builder
            .ins()
            .brif(condition_value.crane, body_block, &[], exit_block, &[]);

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);

        let body_ir = self.translate_expr(*loop_body);
        self.builder.ins().jump(header_block, &[]);

        self.builder.switch_to_block(exit_block);

        // We've reached the bottom of the loop, so there will be no
        // more backedges to the header to exits to the bottom.
        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);

        // Just return 0 for now.
        Val::new(body_ir.crane, body_ir.datatype)
    }

    fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Val {

        let this_func_name = self.current.clone(); 

        let decls = self.module.declarations(); 

        let res = self.module.get_name(name.as_str());
        let res = match res {
            Some(x) => x.clone(), 
            None => {
                Error::report(format!("attempt to call an undefined function with name `{}`, in body definition of function `{}` ", name, this_func_name));
                unreachable!()
            }
        };

        let res = match res {
            FuncOrDataId::Func(f) => f, 
            _ => {
                Error::report(format!("attempt to call an undefined function with name `{}`, in body definition of function `{}` ", name, this_func_name));
                unreachable!()
            }
        };

        let decl = decls.get_function_decl(res);
        let sig = decl.signature.clone(); 




        //let mut sig = self.module.make_signature();

        // Add a parameter for each argument.
        /*for _arg in args.clone() {
            let e = self.translate_expr(_arg); 
            sig.params.push(AbiParam::new(e.datatype));
        }*/

        // For simplicity for now, just make all calls return a single I64.
        //sig.returns.push(AbiParam::new(self.int));

        let arg_len = sig.params.len(); 
        let actual = args.len(); 

        if arg_len != actual {
            Error::report(format!(
                "expected {} parameters when calling function `{}`, but {} parameters were provided", 
                arg_len, 
                name, 
                actual
            ));
        }

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let mut arg_values = Vec::new();
        for (index, arg) in args.into_iter().enumerate() {
            let expr = self.translate_expr(arg); 
            let sig_param = sig.params.get(index).unwrap(); 
            if expr.datatype != sig_param.value_type {
                Error::report(format!(
                    "expected parameter of type {} for parameter number {} in call site of {}, but type {} is provided", 
                    sig_param.value_type,
                    index, 
                    name, 
                    expr.datatype
                ));
            }
            arg_values.push(expr.crane); 
        }

        let call = self.builder.ins().call(local_callee, arg_values.as_slice());
        let crane = self.builder.inst_results(call)[0];
        //TODO: use the actuall function return value
        Val::new(crane, sig.returns.first().unwrap().value_type)
    }

    fn translate_global_data_addr(&mut self, name: String) -> Val {
        let sym = self
            .module
            .declare_data(&name, Linkage::Export, true, false)
            .expect("problem declaring data object");
        let local_id = self.module.declare_data_in_func(sym, self.builder.func);

        let pointer = self.module.target_config().pointer_type();
        let crane = self.builder.ins().symbol_value(pointer, local_id); 
        Val::new(crane, pointer)
    }
}

fn integer_type(t: Type) -> bool {
    match t {
        types::I32 | types::I64 | types::I16 | types::I8 => true, 
        _ => false
    }
}

fn declare_variables(
    int: types::Type,
    builder: &mut FunctionBuilder,
    params: &[String],
    the_return: &str,
    stmts: Expr,
    binds: Vec<(String, String)>,
    mut index: usize, 
    entry_block: Block,
    config: Config, 
) -> HashMap<String, Var> {
    let mut variables = HashMap::new();

    let len = params.len();
    
    if config.defs {
        print!("(");
    }

    for (i, name) in params.iter().enumerate() {
        // TODO: cranelift_frontend should really have an API to make it easy to set
        // up param variables.
        let val = builder.block_params(entry_block)[i];
        let ty = JIT::get_type_mapping_for_name(name.to_string(), binds.clone()).unwrap_or(int); 
        if config.defs {
            print!("{}: {}", name, JIT::type_as_string(ty));
        }
        
        let ty = JIT::type_repr_to_ir(ty); 
        
        let var = declare_variable(ty, builder, &mut variables, &mut index, name);

        if config.defs {
            if i < len - 1 {
                print!(", "); 
            }
        }

        builder.def_var(var.crane, val);
    }

    let ty = JIT::get_type_mapping_for_name(the_return.to_string(), binds).unwrap_or(int); 
    if config.defs {
        print!(") -> ");
        println!("{}: {}", the_return, JIT::type_as_string(ty)); 
    }

    let ty = JIT::type_repr_to_ir(ty); 
    
    let return_variable = declare_variable(ty, builder, &mut variables, &mut index, the_return);
    let val = if integer_type(ty) {
        builder.ins().iconst(ty, 0) 
    } else {
        match ty {
            types::F32 => builder.ins().f32const(0 as f32), 
            types::F64 => builder.ins().f64const(0 as f64), 
            _ => panic!("found unknown type"), 
        }
    }; 


    builder.def_var(return_variable.crane, val);
    declare_variables_in_stmt(int, builder, &mut variables, &mut index, &stmts);

    variables
}

/// Recursively descend through the AST, translating all implicit
/// variable declarations.
fn declare_variables_in_stmt(
    int: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, Var>,
    index: &mut usize,
    expr: &Expr,
) {
    match expr.clone() {
        Expr::Assign(ref name, _) => {
            declare_variable(int, builder, variables, index, name);
        }
        Expr::IfElse(ref _condition, ref then_body, ref else_body) => {
            declare_variables_in_stmt(int, builder, variables, index, then_body);
            declare_variables_in_stmt(int, builder, variables, index, else_body);
        }
        Expr::WhileLoop(ref _condition, ref loop_body) => {
            declare_variables_in_stmt(int, builder, variables, index, loop_body);
        }
        Expr::Block(stmts) => {
            for stmt in stmts {
                declare_variables_in_stmt(int, builder, variables, index, &stmt);
            }
        }
        _ => (),
    }
}

/// Declare a single variable declaration.
fn declare_variable(
    int: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, Var>,
    index: &mut usize,
    name: &str,
) -> Var {
    let crane = Variable::new(*index);
    let var = Var::new(crane, int); 
    if !variables.contains_key(name) {
        variables.insert(name.into(), var.clone());
        builder.declare_var(crane, int);
        *index += 1;
    }
    var
}
