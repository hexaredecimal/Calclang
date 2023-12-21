
use cranelift::prelude::*;
use cranelift::prelude::Type; 

/// IR Value type
#[derive(Debug, Clone)]
pub struct Val {
    /// Used for values returned by the ir
    pub crane: Value, 

    /// Datatype of the last operation
    pub datatype: Type
}

impl Val {

    /// Creates a new instance of the Val
    pub fn new(crane: Value, datatype: Type) -> Self {
        Self {
            crane, 
            datatype
        }
    }
}

/// IR Var type
#[derive(Debug, Clone)]
pub struct Var {

    /// IR representation of the variable
    pub crane: Variable,

    /// The datatype of the variable
    pub datatype: Type
}

impl Var {
    /// Create a new instance of the Var structure
    pub fn new(crane: Variable, datatype: Type) -> Self {
        Self {
            crane, 
            datatype
        }
    }
}



/// IR Func type
pub struct Func {
    /// Params for the function
    pub params: Vec<AbiParam>, 

    /// Return for the function
    pub returns: Vec<AbiParam>
}


impl Func {
    
    /// Creates a new instance of the Func
    pub fn new(params: Vec<AbiParam>, returns: Vec<AbiParam>) -> Self {
        Self {
            params, 
            returns
        }
    }
}

