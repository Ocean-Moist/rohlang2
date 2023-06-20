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
            'd' => {
                chars.next();
                match chars.peek() {
                    Some(&'u') => {
                        chars.next();
                        match chars.peek() {
                            Some(&'p') => {
                                chars.next();
                                tokens.push(Tokens::Dup);
                            }
                            _ => panic!("Invalid character"),
                        }
                    }
                    _ => panic!("Invalid character"),
                }
            }
            '0'..='9' => {
                let mut number = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() {
                        chars.next();
                        number.push(c);
                    } else {
                        break;
                    }
                }
                let parsed_number: i32 = number.parse().unwrap();
                tokens.push(Tokens::Val(parsed_number));
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
                println!("xz: {:?}, yz: {:?}", xz, yz);
                ast = replace_input_node(xz, yz);
            }
            Tokens::K(x, _) => {
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
    if args[1] == "repl" {
        repl();
    }
    // open .roh file and read it into a string
    let input = std::fs::read_to_string(&args[1]).expect("Failed to read file");
    let tokens = lexer(&input); // Tokenize input string
    let ast = parser(&tokens); // Parse tokens into AST
    println!("{:?}", eval(ast, &mut None))
}

fn repl() -> () {
    loop {
        let std = std::io::stdin();
        let mut input: String = "".to_string();
        std.read_line(&mut input);

        let tokens = lexer(&input);
        let ast = parser(&tokens);
        println!("tokens: {:?}", tokens);
        println!("ast: {:?}", ast);
        println!("out: {:?}", eval(ast, &mut None));
    }
}
