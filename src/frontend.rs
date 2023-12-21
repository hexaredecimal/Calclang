/*
                                 Apache License
                           Version 2.0, January 2004
                        http://www.apache.org/licenses/

   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION
        copyright 2018 cranelift developers

        Modified by: Gama Sibusiso Vincent

*/


/// The AST node for expressions.
#[derive(Debug, Clone)]
pub enum Expr {
    /// i32 integer literal 
    IntLiteral(String),

    /// f32 literal
    FloatLiteral(String),

    /// string literal 
    StringLiteral(String),

    /// An identifier
    Identifier(String),

    /// Variable assignment
    Assign(String, Box<Expr>),

    /// Expression comparison
    Eq(Box<Expr>, Box<Expr>),

    /// Same as above 
    Ne(Box<Expr>, Box<Expr>),
    
    /// Same as above 
    Lt(Box<Expr>, Box<Expr>),
    
    /// Same as above 
    Le(Box<Expr>, Box<Expr>),
    
    /// Same as above 
    Gt(Box<Expr>, Box<Expr>),
    
    /// Same as above 
    Ge(Box<Expr>, Box<Expr>),
    
    /// Add two expressions
    Add(Box<Expr>, Box<Expr>),

    /// Subtracts two expressions
    Sub(Box<Expr>, Box<Expr>),
    
    /// Multiply two expressions
    Mul(Box<Expr>, Box<Expr>),
    
    /// Divide two expressions
    Div(Box<Expr>, Box<Expr>),
    
    /// Use mod on two expressions
    Mod(Box<Expr>, Box<Expr>),
    
    /// Choose a branch based on a condition
    IfElse(Box<Expr>, Box<Expr>, Box<Expr>),
    
    /// Loop and repeat an expression, while an expression is true
    WhileLoop(Box<Expr>, Box<Expr>),

    /// A function call
    Call(String, Vec<Expr>),

    /// Get a pointer to a variable
    GlobalDataAddr(String),

    /// A block of statements, returns the last statement
    Block(Vec<Expr>), 

    /// Used for type binding
    Where(Vec<(String, String)>), 

    /// variable binding
    Let(String, String, Box<Expr>), 

    /// Branch and execute an expression based on a condition
    Match(Box<Expr>, Vec<(Box<Expr>, Box<Expr>)>), 

    /// Choose a value between the two
    Unary(Box<Expr>, Box<Expr>), 

    /// Variable increment short-hand
    IncBy(Box<Expr>, Box<Expr>), 

    /// Variable decrement short-hand
    DecBy(Box<Expr>, Box<Expr>), 

    /// Variable multiply short-hand
    MulBy(Box<Expr>, Box<Expr>),

    /// Variable division short-hand
    DivBy(Box<Expr>, Box<Expr>),

    /// Bitwise or
    BitOr(Box<Expr>, Box<Expr>),

    /// Bitwise and
    BitAnd(Box<Expr>, Box<Expr>),

    /// Bitwise Xor
    BitXor(Box<Expr>, Box<Expr>), 
    
    /// Bitwise Not
    BitNot(Box<Expr>),

    /// Shift left
    Shl(Box<Expr>, Box<Expr>), 
    
    /// Shift right
    Shr(Box<Expr>, Box<Expr>), 
    
    /// Negative expression
    Neg(Box<Expr>),

    /// Size of an expression
    SizeOf(Box<Expr>)
}

/// AST node for statements
#[derive(Debug, Clone)]
pub enum Statement {
    Function(String, Vec<String>, String, Expr, Expr),
    Import(Vec<String>),
    Comment
}


peg::parser!(pub grammar parser() for str {

    /// Parses top level statements [functions | imports | comment]
    pub rule top_levels() -> Vec<Statement>
        = _ s:(top_level() ** (_)) { s }

    rule top_level() -> Statement 
        = function()
        / import()
        / comment()

    rule import() -> Statement 
        = "using" _ module:identifier() _ "::" _ i:imports() _ { 
            let mut v = vec![module]; 
            let mut v2 = i.clone();  
            v.append(&mut v2); 
            Statement::Import(v)
        } 

    rule imports() -> Vec<String> 
        = "{" _ s:(identifier() ** ("," _)) _ "}" { s }
        / s:identifier() _ { vec![s]}
        / "*" _ { vec!["*".to_string()] }


    rule comment() -> Statement
        = "//" line_comment_() _ { Statement::Comment }


    rule line_comment_()
        = s:([^'\n']+) 


    rule function() -> Statement
        = "fn" _ name:identifier() _
        "(" params:((_ i:identifier() _ {i}) ** ",") ")" _
        "->" _
        "(" returns:(_ i:identifier() _ {i}) ")" t:type_decls()
        "=" _ 
        body:expression()
        _
        { Statement::Function(name, params, returns, body, t) }

    rule let_decl() -> Expr 
        = "let" _ a:identifier() t:type_decl()  "=" _ e:expression() _ { Expr::Let(a, t, Box::new(e)) }


    rule match_decl() -> Expr
        = "match" _ e:expression() _ "{" _
        loop_body:match_cases() _ "}" _ 
        { Expr::Match(Box::new(e), loop_body) }


    rule match_cases() -> Vec<(Box<Expr>, Box<Expr>)> 
        = a:(match_case() **("," _)) { a }

    rule match_case() -> (Box<Expr>, Box<Expr>) 
        = e:expression() _ "=>" _  c:expression() { (Box::new(e), Box::new(c)) }


    rule type_decl() -> String
        = _ ":" _ i:identifier() _ { i }
        / _ { "".to_string() }

    rule block() -> Expr 
        = "{" _ s:statements() _ "}" { Expr::Block(s) }

    rule type_decls() -> Expr 
        = _ s:where_clause() _ { s }
        / _ { Expr::Where(vec![("".to_string(), "".to_string())])}

    rule where_clause() -> Expr 
        = _ ":" _ "where" _ types:(type_binding() ** ("," _)) { Expr::Where(types)}

    rule type_binding() -> (String, String)
        = a:identifier() _ "->" _ b:identifier() { (a, b) }


    rule statements() -> Vec<Expr>
        = s:(statement()*) { s }

    rule statement() -> Expr
        = _ e:expression() _ { e }

    rule expression() -> Expr
        = if_else()
        / let_decl()
        / match_decl()
        / while_loop()
        / assignment()
        / binary_op()
        / block()
        / not()


    rule if_else() -> Expr
        = "if" _ e:expression() _
        then_body:expression() _ "else" _
        else_body:expression() _ 
        { Expr::IfElse(Box::new(e), Box::new(then_body), Box::new(else_body)) }

    rule while_loop() -> Expr
        = "while" _ e:expression() _ 
        loop_body:expression() _
        { Expr::WhileLoop(Box::new(e), Box::new(loop_body)) }

    rule assignment() -> Expr
        = i:identifier() _ "=" _ e:expression() {Expr::Assign(i, Box::new(e))}

    rule binary_op() -> Expr = precedence!{
        a:@ _ "==" _ b:(@) { Expr::Eq(Box::new(a), Box::new(b)) }
        a:@ _ "!=" _ b:(@) { Expr::Ne(Box::new(a), Box::new(b)) }
        a:@ _ "<<" _ b:(@) { Expr::Shl(Box::new(a), Box::new(b)) }
        a:@ _ "<"  _ b:(@) { Expr::Lt(Box::new(a), Box::new(b)) }
        a:@ _ "<=" _ b:(@) { Expr::Le(Box::new(a), Box::new(b)) }
        a:@ _ ">>" _ b:(@) { Expr::Shr(Box::new(a), Box::new(b)) }
        a:@ _ ">"  _ b:(@) { Expr::Gt(Box::new(a), Box::new(b)) }
        a:@ _ ">=" _ b:(@) { Expr::Ge(Box::new(a), Box::new(b)) }

        a:@ _ "+=" _ b:(@) { Expr::IncBy(Box::new(a), Box::new(b)) }
        a:@ _ "-=" _ b:(@) { Expr::DecBy(Box::new(a), Box::new(b)) }
        a:@ _ "/=" _ b:(@) { Expr::MulBy(Box::new(a), Box::new(b)) }
        a:@ _ "*=" _ b:(@) { Expr::DivBy(Box::new(a), Box::new(b)) }
        --
        a:@ _ "+" _ b:(@) { Expr::Add(Box::new(a), Box::new(b)) }
        a:@ _ "-" _ b:(@) { Expr::Sub(Box::new(a), Box::new(b)) }
        --
        a:@ _ "*" _ b:(@) { Expr::Mul(Box::new(a), Box::new(b)) }
        a:@ _ "/" _ b:(@) { Expr::Div(Box::new(a), Box::new(b)) }
        a:@ _ "%" _ b:(@) { Expr::Mod(Box::new(a), Box::new(b)) }
        a:@ _ "??" _ b:(@) { Expr::Unary(Box::new(a), Box::new(b)) }


        --
        a:@ _ "&" _ b:(@) { Expr::BitAnd(Box::new(a), Box::new(b)) }
        a:@ _ "|" _ b:(@) { Expr::BitOr(Box::new(a), Box::new(b)) }
        a:@ _ "?" _ b:(@) { Expr::BitXor(Box::new(a), Box::new(b)) }
        --
         "(" _ e:expression() _ ")" _ { e }
        i:identifier() _ "(" args:((_ e:expression() _ {e}) ** ",") ")" { Expr::Call(i, args) }
        i:identifier() { Expr::Identifier(i) }
        l:literal() { l }
    }

    rule not() -> Expr 
        = "!" _ e:expression() { Expr::BitNot(Box::new(e)) }
        / "-" _ e:expression() { Expr::Neg(Box::new(e)) }
        / "@" _ e:expression() { Expr::SizeOf(Box::new(e)) }

    rule string_literal() -> String
        = "\"" s:$([^'"']) * "\""{ 
        let mut st = String::new(); 
        for v in s {
            st.push_str(v); 
        }
        st
    }

    rule number() -> String 
        = n:$(['0'..='9']+) { n.to_string() }

    rule float() -> String 
        = l:number() "." r:$(['0'..='9']*) {
            let mut s = l.to_string();
            s.push_str(r); 
            s
        }

    rule identifier() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { n.to_owned() } }
        / expected!("identifier")

    rule literal() -> Expr
        = n:number() { Expr::IntLiteral(n.to_owned()) }
        / n:float() { Expr::FloatLiteral(n.to_owned()) }
        / "&" i:identifier() { Expr::GlobalDataAddr(i) }
        / s:string_literal() { Expr::StringLiteral(s) }


    rule _() =  quiet!{[' ' | '\t' | '\n' | '\r']*}
});
