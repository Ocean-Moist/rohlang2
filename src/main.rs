use std::collections::VecDeque;
use std::fmt;
use std::iter::Peekable;
use std::str::FromStr;
use crate::LexerError::InvalidKeyword;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum Ops {
    Add,
    Sub,
    Div,
    Mul,
    Pow,
    Min,
    Max,
    And,
    Or,
    Not,
    Mod,
    Eq,
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
enum Tokens {
    Op(Ops),
    S(Box<Tokens>, Box<Tokens>, Box<Tokens>),
    K(Box<Tokens>, Box<Tokens>),
    Dup,
    Val(i32),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum AST {
    Operator(Ops, Box<AST>, Box<AST>),
    Input,
    Dup,
    Val(i32),
}
fn display_tree(ast: &AST, prefix: &str, is_last: bool) {
    let connector = if is_last { "└── " } else { "├── " };
    let mut prefix_child = String::from(prefix);
    if is_last {
        prefix_child.push_str("    ");
    } else {
        prefix_child.push_str("│   ");
    }

    match ast {
        AST::Operator(op, left, right) => {
            println!("{}{:?}", prefix, op);
            display_tree(left, &prefix_child, false);
            display_tree(right, &prefix_child, true);
        }
        AST::Input => println!("{}Input", connector),
        AST::Dup => println!("{}Dup", connector),
        AST::Val(val) => println!("{}Val({})", connector, val),
    }
}

#[derive(Debug)]
enum LexerError {
    InvalidCharacter(char),
    InvalidNumber(String),
    InvalidKeyword(String),
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexerError::InvalidCharacter(c) => write!(f, "Invalid character: {}", c),
            LexerError::InvalidNumber(s) => write!(f, "Invalid number: {}", s),
            InvalidKeyword(s) => write!(f, "Invalid keyword: {}", s)
        }
    }
}

impl FromStr for Ops {
    type Err = LexerError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "min" => Ok(Ops::Min),
            "max" => Ok(Ops::Max),
            _ => Err(InvalidKeyword(s.to_string()))
        }
    }
}


fn lexer(input: &str) -> Result<Vec<Tokens>, LexerError> {
    let mut tokens: Vec<Tokens> = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            ' ' | '\n' => {
                chars.next(); // Ignore whitespace and newlines
            }
            '+' | '-' | '*' | '/' | '^' | '&' | '|' | '!' | '%' | '=' => {
                tokens.push(Tokens::Op(lex_op(c)));
                chars.next();
            }
            'a'..='z' => {
                let keyword = lex_keyword(&mut chars)?;
                match keyword.as_str() {
                    "s" => tokens.push(lex_s(&mut chars)?),
                    "k" => tokens.push(lex_k(&mut chars)?),
                    "dup" => tokens.push(Tokens::Dup),
                    _ => tokens.push(Tokens::Op(Ops::from_str(&keyword)?)),
                }
            }
            '0'..='9' => {
                tokens.push(lex_number(&mut chars)?);
            }
            _ => return Err(LexerError::InvalidCharacter(c)),
        }
    }
    Ok(tokens)
}

fn lexer_single<I>(iter: &mut Peekable<I>) -> Result<Tokens, LexerError>
    where
        I: Iterator<Item = char>,
{
    match iter.peek() {
        Some(&c) if c.is_ascii_alphabetic() => {
            let keyword = lex_keyword(iter)?;
            match keyword.as_str() {
                "s" => Ok(lex_s(iter)?),
                "k" => Ok(lex_k(iter)?),
                "dup" => Ok(Tokens::Dup),
                _ => {
                    Err(InvalidKeyword(keyword))
                },
            }
        }
        Some(&c) if is_operator(c)  => Ok(Tokens::Op(lex_op(c))),
        Some(&c) => Err(LexerError::InvalidCharacter(c)),
        None => Err(InvalidKeyword("".to_string())),
    }
}

fn is_operator(op: char) -> bool {
    match op {
        '+' | '-' | '*' | '/' | '^' | '&' | '|' | '!' | '%' | '=' => true,
        _ => false,
    }
}

fn lex_keyword<I>(iter: &mut Peekable<I>) -> Result<String, LexerError>
    where
        I: Iterator<Item = char>,
{
    let mut keyword = String::new();
    while let Some(&c) = iter.peek() {
        if c.is_ascii_alphabetic() {
            iter.next();
            keyword.push(c);
        } else {
            break;
        }
    }
    if keyword.is_empty() {
        Err(InvalidKeyword(keyword))
    } else {
        Ok(keyword)
    }
}

fn lex_s<I>(iter: &mut Peekable<I>) -> Result<Tokens, LexerError>
    where
        I: Iterator<Item = char>,
{
    let s1 = lexer_single(iter)?;
    iter.next(); 
    let s2 = lexer_single(iter)?;
    iter.next();
    let s3 = lexer_single(iter)?;
    iter.next();
    Ok(Tokens::S(Box::new(s1), Box::new(s2), Box::new(s3)))
}

fn lex_k<I>(iter: &mut Peekable<I>) -> Result<Tokens, LexerError>
    where
        I: Iterator<Item = char>,
{
    let k1 = lexer_single(iter)?;
    iter.next();
    let k2 = lexer_single(iter)?;
    iter.next();
    Ok(Tokens::K(Box::new(k1), Box::new(k2)))
}

fn lex_number<I>(iter: &mut Peekable<I>) -> Result<Tokens, LexerError>
    where
        I: Iterator<Item = char>,
{
    let mut number = String::new();
    while let Some(&c) = iter.peek() {
        if c.is_ascii_digit() {
            iter.next();
            number.push(c);
        } else {
            break;
        }
    }
    let parsed_number: i32 = number.parse().map_err(|_| LexerError::InvalidNumber(number))?;
    Ok(Tokens::Val(parsed_number))
}

fn lex_op(c: char) -> Ops {
    match c {
        '+' => Ops::Add,
        '-' => Ops::Sub,
        '*' => Ops::Mul,
        '/' => Ops::Div,
        '^' => Ops::Pow,
        '&' => Ops::And,
        '|' => Ops::Or,
        '!' => Ops::Not,
        '%' => Ops::Mod,
        '=' => Ops::Eq,
        _ => panic!("Invalid operator"),
    }
}

fn unbox_tokens(tokens: &Box<Tokens>) -> Vec<Tokens> {
    let mut result: Vec<Tokens> = Vec::new();

    match &**tokens {
        Tokens::Op(op) => {
            result.push(Tokens::Op(op.clone()));
        }
        Tokens::S(ref t1, ref t2, ref t3) => {
            result.extend_from_slice(&unbox_tokens(t1));
            result.extend_from_slice(&unbox_tokens(t2));
            result.extend_from_slice(&unbox_tokens(t3));
        }
        Tokens::K(ref t1, ref t2) => {
            result.extend_from_slice(&unbox_tokens(t1));
            result.extend_from_slice(&unbox_tokens(t2));
        }
        Tokens::Dup => {
            result.push(Tokens::Dup);
        }
        Tokens::Val(x) => {
            result.push(Tokens::Val(*x));
        }
    }

    result
}

fn parser(tokens: &Vec<Tokens>) -> AST {
    let mut ast: AST = AST::Input; // Initially set to Input to indicate an empty expression

    for token in tokens.iter() {
        match token {
            Tokens::S(x, y, z) => {
                let xz = replace_input_node(parser(&unbox_tokens(x)), parser(&unbox_tokens(z)));
                let yz = replace_input_node(parser(&unbox_tokens(y)), parser(&unbox_tokens(z)));
                println!("x: {:?}, y: {:?}, z: {:?}", x, y, z);
                println!("xz: {:?}, yz: {:?}", xz, yz);
                ast = replace_input_node(xz, yz);
            }
            Tokens::K(x, y) => {
                println!("x: {:?} y: {:?}", x, y);
                ast = replace_input_node(ast, parser(&unbox_tokens(x)));
            }
            Tokens::Op(op) => {
                ast = replace_input_node(ast, generate_op_node_structure(op.clone()));
            }
            Tokens::Dup => {
                ast = replace_next_input_node_with_dupe(ast);
            }
            Tokens::Val(x) => {
                ast = replace_input_node(ast, AST::Val(*x));
            }
        }
    }

    ast
}

fn replace_next_input_node_with_dupe(ast: AST) -> AST {
    // look for 2 input nodes in a row and replace the last one with Dup
    let mut queue = VecDeque::new();
    queue.push_back(ast.clone());

    while let Some(node) = queue.pop_front() {
        match node {
            AST::Operator(op, left, right) => match (&*left, &*right) {
                (&AST::Input | &AST::Val(_), &AST::Input) => {
                    return AST::Operator(op, left, Box::new(AST::Dup));
                }
                _ => {
                    queue.push_back(*left);
                    queue.push_back(*right);
                }
            },
            AST::Val(_) => {
                panic!("dupe needs two inputs")
            }
            AST::Input => {
                panic!("dupe needs two inputs")
            }
            AST::Dup => {
                panic!("dupe needs two inputs")
            }
        }
    }

    ast
}



fn replace_input_node(ast: AST, input: AST) -> AST {
    let mut queue = VecDeque::new();
    queue.push_back(ast.clone());

    while let Some(node) = queue.pop_front() {
        match node {
            AST::Operator(op, left, right) => {
                if *left == AST::Input {
                    return AST::Operator(op, Box::new(input.clone()), right);
                } else if *right == AST::Input {
                    return AST::Operator(op, left, Box::new(input.clone()));
                } else {
                    queue.push_back(*left);
                    queue.push_back(*right);
                }
            }
            AST::Input => {
                return input.clone();
            }
            AST::Dup => {
                return AST::Dup;
            }
            AST::Val(x) =>  {
                return AST::Val(x);
            }
        }
    }

    ast
}

fn generate_op_node_structure(op: Ops) -> AST {
    match op {
        Ops::Add => AST::Operator(Ops::Add, Box::new(AST::Input), Box::new(AST::Input)),
        Ops::Sub => AST::Operator(Ops::Sub, Box::new(AST::Input), Box::new(AST::Input)),
        Ops::Mul => AST::Operator(Ops::Mul, Box::new(AST::Input), Box::new(AST::Input)),
        Ops::Div => AST::Operator(Ops::Div, Box::new(AST::Input), Box::new(AST::Input)),
        Ops::Pow => AST::Operator(Ops::Pow, Box::new(AST::Input), Box::new(AST::Input)),
        Ops::Min => AST::Operator(Ops::Min, Box::new(AST::Input), Box::new(AST::Input)),
        Ops::Max => AST::Operator(Ops::Max, Box::new(AST::Input), Box::new(AST::Input)),
        Ops::And => AST::Operator(Ops::And, Box::new(AST::Input), Box::new(AST::Input)),
        Ops::Or => AST::Operator(Ops::Or, Box::new(AST::Input), Box::new(AST::Input)),
        Ops::Not => AST::Operator(Ops::Not, Box::new(AST::Input), Box::new(AST::Input)),
        Ops::Mod => AST::Operator(Ops::Mod, Box::new(AST::Input), Box::new(AST::Input)),
        Ops::Eq => AST::Operator(Ops::Eq, Box::new(AST::Input), Box::new(AST::Input)),
    }
}

fn eval(ast: AST, last: &mut Option<i32>) -> i32 {
    match ast {
        AST::Operator(op, left, right) => {
            let left_value = eval(*left, last);
            let right_value = eval(*right, last);

            match op {
                Ops::Add => left_value + right_value,
                Ops::Sub => left_value - right_value,
                Ops::Mul => left_value * right_value,
                Ops::Div => left_value / right_value,
                Ops::Pow => left_value.pow(right_value as u32),
                Ops::Min => left_value.min(right_value),
                Ops::Max => left_value.max(right_value),
                Ops::And => (left_value != 0 && right_value != 0) as i32,
                Ops::Or => (left_value != 0 || right_value != 0) as i32,
                Ops::Not => (left_value == 0) as i32,
                Ops::Mod => left_value % right_value,
                Ops::Eq => (left_value == right_value) as i32,
            }
        }
        AST::Input => loop {
            let mut input = String::new();

            std::io::stdin()
                .read_line(&mut input)
                .expect("Failed to read input");
            if let Ok(value) = input.trim().parse::<i32>() {
                *last = Some(value);
                break value;
            } else {
                println!("Invalid input! Please enter a valid integer.");
            }
        },
        AST::Dup => match last {
            Some(last) => *last,
            None => panic!("No input to duplicate!"),
        },
        AST::Val(x) => {
            *last = Some(x);
            x
        }
    }
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() < 2 {
        println!("Usage: roh <file.roh> or roh repl");
        return;
    }
    if args[1] == "repl" {
        repl();
    }
    // open .roh file and read it into a string
    let input = std::fs::read_to_string(&args[1]).expect("Failed to read file");
    let tokens = lexer(&input); // Tokenize input string
    let tokens = match tokens {
        Ok(tokens) => tokens,
        Err(e) => {
            println!("LexerError: {}", e);
            return;
        }
    };
    let ast = parser(&tokens); // Parse tokens into an AST
    println!("ast: {:?}", ast); // Display the AST (for debugging purposes
    display_tree(&ast, "", false); // Display the AST (for debugging purposes
    println!("{:?}", eval(ast, &mut None))
}
// xz: Operator(Sub, Operator(Sub, Input, Input), Input), yz: Operator(Sub, Operator(Sub, Input, Input), Input)
// xz: Operator(Sub, Operator(Mul, Input, Input), Input), yz: Operator(Add, Operator(Mul, Input, Input), Input)
// ast: Operator(Sub, Operator(Mul, Input, Input), Operator(Add, Operator(Mul, Input, Input), Input))
// 1 - 

fn repl() -> () {
    loop {
        let std = std::io::stdin();
        let mut input: String = "".to_string();
        std.read_line(&mut input).unwrap();

        let tokens = lexer(&input);
        let tokens = match tokens {
            Ok(tokens) => tokens,
            Err(e) => {
                println!("LexerError: {}", e);
                continue;
            }
        };
        let ast = parser(&tokens);
        println!("tokens: {:?}", tokens);
        println!("ast: {:?}", ast);
        println!("out: {:?}", eval(ast, &mut None));
    }
}
