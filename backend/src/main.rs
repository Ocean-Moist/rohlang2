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
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum AST {
    Operator(Ops, Box<AST>, Box<AST>),
    Input,
}

fn lexer(input: &str) -> Vec<Tokens> {
    let mut tokens: Vec<Tokens> = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            ' ' => {
                chars.next(); // Ignore whitespace
            }
            '\n' => {
                chars.next(); // Ignore newlines
            }
            '+' | '-' | '*' | '/' | '^' | '&' | '|' | '!' | '%' | '=' => {
                tokens.push(Tokens::Op(lex_op(c)));
                chars.next();
            }
            'm' => {
                chars.next();
                match chars.peek() {
                    Some(&'i') => {
                        chars.next();
                        match chars.peek() {
                            Some(&'n') => {
                                chars.next();
                                tokens.push(Tokens::Op(Ops::Min));
                            }
                            _ => panic!("Invalid character"),
                        }
                    }
                    Some(&'a') => {
                        chars.next();
                        match chars.peek() {
                            Some(&'x') => {
                                chars.next();
                                tokens.push(Tokens::Op(Ops::Max));
                            }
                            _ => panic!("Invalid character"),
                        }
                    }
                    _ => panic!("Invalid character"),
                }
            }
            's' => {
                chars.next();
                let s1 = Box::new(lexer_single(&mut chars));
                let s2 = Box::new(lexer_single(&mut chars));
                let s3 = Box::new(lexer_single(&mut chars));
                tokens.push(Tokens::S(s1, s2, s3));
            }
            'k' => {
                chars.next();
                let k1 = Box::new(lexer_single(&mut chars));
                let k2 = Box::new(lexer_single(&mut chars));
                tokens.push(Tokens::K(k1, k2));
            }
            _ => panic!("Invalid character"),
        }
    }

    tokens
}

fn lexer_single<I>(iter: &mut I) -> Tokens
where
    I: Iterator<Item = char>,
{
    if let Some(c) = iter.next() {
        match c {
            ' ' | '\n' => lexer_single(iter),
            '+' | '-' | '*' | '/' | '^' | '&' | '|' | '!' | '%' | '=' => Tokens::Op(lex_op(c)),
            'm' => match iter.next() {
                Some('i') => {
                    if let Some('n') = iter.next() {
                        Tokens::Op(Ops::Min)
                    } else {
                        panic!("Invalid character")
                    }
                }
                Some('a') => {
                    if let Some('x') = iter.next() {
                        Tokens::Op(Ops::Max)
                    } else {
                        panic!("Invalid character")
                    }
                }
                _ => panic!("Invalid character"),
            },
            's' => {
                let s1 = Box::new(lexer_single(iter));
                let s2 = Box::new(lexer_single(iter));
                let s3 = Box::new(lexer_single(iter));
                Tokens::S(s1, s2, s3)
            }
            'k' => {
                let k1 = Box::new(lexer_single(iter));
                let k2 = Box::new(lexer_single(iter));
                Tokens::K(k1, k2)
            }
            _ => panic!("Invalid character"),
        }
    } else {
        panic!("Invalid character")
    }
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
    }

    result
}

fn parser(tokens: &Vec<Tokens>) -> AST {
    let mut ast: AST = AST::Input; // Initially set to Input to indicate an empty expression

    for (_, token) in tokens.iter().enumerate() {
        match token {
            Tokens::S(x, y, z) => {
                let xz = replace_input_node(parser(&unbox_tokens(x)), parser(&unbox_tokens(z)));
                let yz = replace_input_node(parser(&unbox_tokens(y)), parser(&unbox_tokens(z)));
                println!("xz: {:?}, yz: {:?}", xz, yz);
                ast = replace_input_node(xz, yz);
            }
            Tokens::K(x, _) => {
                ast = replace_input_node(ast, parser(&unbox_tokens(x)));
            }
            Tokens::Op(op) => {
                ast = replace_input_node(ast, generate_op_node_structure(op.clone()));
            }
        }
    }

    ast
}

use std::collections::VecDeque;

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

fn eval(ast: AST) -> i32 {
    match ast {
        AST::Operator(op, left, right) => {
            let left_value = eval(*left);
            let right_value = eval(*right);
            match op {
                Ops::Add => left_value + right_value,
                Ops::Sub => left_value - right_value,
                Ops::Mul => left_value * right_value,
                Ops::Div => left_value / right_value,
                Ops::Pow => left_value.pow(right_value as u32),
                Ops::Min => left_value.min(right_value),
                Ops::Max => left_value.max(right_value),
                Ops::And => {
                    if left_value != 0 && right_value != 0 {
                        1
                    } else {
                        0
                    }
                }
                Ops::Or => {
                    if left_value != 0 || right_value != 0 {
                        1
                    } else {
                        0
                    }
                }
                Ops::Not => {
                    if left_value == 0 {
                        1
                    } else {
                        0
                    }
                }
                Ops::Mod => left_value % right_value,
                Ops::Eq => {
                    if left_value == right_value {
                        1
                    } else {
                        0
                    }
                }
            }
        }
        AST::Input => {
            // Prompt the user for input until a valid integer is entered
            loop {
                let mut input = String::new();
                std::io::stdin()
                    .read_line(&mut input)
                    .expect("Failed to read input");
                match input.trim().parse::<i32>() {
                    Ok(value) => break value,
                    Err(_) => println!("Invalid input! Please enter a valid integer."),
                }
            }
        }
    }
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    // open .roh file and read it into a string
    let input = std::fs::read_to_string(&args[1]).expect("Failed to read file");
    let tokens = lexer(&input); // Tokenize input string
    let ast = parser(&tokens); // Parse tokens into AST
    println!("{:?}", eval(ast))
}
