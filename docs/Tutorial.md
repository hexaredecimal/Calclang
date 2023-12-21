
# ZULU programming lang tutorial  

## Introduction

Welcome to the `Zulu lang programming tutorial`. This document is meant to get you up and running with the zulu programming language. 
From now onwards I will use the word `Zulu` to refer to the programming language. I wish you a great time using zulu for your programming purposes.

### What is Zulu?  (TODO)
>> "Zulu is a small general purpose, strictly typed, JIT compiled, multiparadigm programming creted as a continuation of the cranelift demo."


### Hello World (Todo)

### Functions 
>> Since Zulu follows a strict main entry point, there are no global variables. Functions are the only type of declaration that gets compiled 
on the top level. To declare a function use the `fn` keyword follwed by the rest of the definition. see below:
`
fn add(x, y) -> (c)
    :where x -> i32, y -> i32, c -> i32 = 
        c = x + y
`
>> Woah, thats a lot for a start? Well its actually very simple if you look at it. If Zulu is not your first language then your will not have a 
hard time figuring out what is going on, but let me explain anyway. We declared a function named `add` using the `fn` keyword. Our function 
takes two input parameters, named `x` and `y` and one output parameter named `c`. The `where` keyword is used to bind the parameters to user types. 
All functions have `i64` as the default type for input and output parameters. Here we bind the input parameters to the types `i32` for `x` and the same for `y` and `c`.
The output parameter is the return variable. It can be initialized and used in expressions inside the function body. Speaking of the function body, 
it is actually an expressions. Expressions are flexible in Zulu. See the next section. 

### Expressions
>> Zulu has different types of expressions, all can be assigned to variables and returned from functions. Just to state something weird, loops are also expressions that have type `i64`. 
All expressions have types in Zulu. Everything except top level statements are expressions in zulu. 

### Literals
>> Zulu supports integer literals, by default they have type i32. 
    Example:
        `   5
            200
        `

### Operators
>> Zulu also supports most common binary, logical and bitwise operators

    Binary(x, y): -> (z)
        :where x -> i32, y -> i32, z -> i32 

            +   -> adds two input values 
            -   -> subtracts two input values 
            *   -> multiply two input values 
            /   -> divivde two input values 
            %   -> get the remainder of two input values 
        
    Unary(x) -> (y)
        : where x -> i32, y -> i32

            !   -> a bitwise not on one input value
            -   -> negates a value
    
    Logical(x, y): -> (z)
        :where x -> i32, y -> i32, z -> i32 

            ==  -> checks if two input values are equal
            !=  -> checks if two input values are not
            <   -> checks if one input is less than the other 
            <=  -> checks if one input is less or equal to the other
            >   -> checks if one input is greater than the other 
            >=  -> checks if one input is greater or equal to the other
            &&  -> does a logical and on the two inputs 
            ||  -> does a logical or on the two inputs 
            ??  -> unary operator


    Bitwise(x, y): -> (z)
        :where x -> i32, y -> i32, z -> i32 
            & -> does a bitwise and operation
            | -> does a bitwise or operators
            ? -> does a bitwise xor operators
 

