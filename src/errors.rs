
/// Structure for report errors
pub struct Error; 

impl Error {

    /// Report compilation errors
    pub fn report(message: String) {
        println!("Error: {}", message);
        std::process::exit(0); 
    } 
}

