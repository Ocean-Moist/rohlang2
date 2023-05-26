#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum Combinators {
    S,
    K,
}

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
    Eq
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
enum Tokens {
    Combinator(Combinators),
    Op(Ops),
    OpenParen,
    CloseParen,
} 

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum AST {
    Operator(Ops, Box<AST>, Box<AST>),
    Combinator(Combinators, Box<AST>, Box<AST>),
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
            '(' => {
                tokens.push(Tokens::OpenParen);
                chars.next();
            }
            ')' => {
                tokens.push(Tokens::CloseParen);
                chars.next();
            }
            '+' | '-' | '*' | '/' | '^' | '&' | '|' | '!' | '%' | '=' => {
                tokens.push(Tokens::Op(parse_op(c)));
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
                tokens.push(Tokens::Combinator(Combinators::S));
                chars.next();
            }
            'k' => {
                tokens.push(Tokens::Combinator(Combinators::K));
                chars.next();
            }
            _ => panic!("Invalid character"),
        }
    }

    tokens
}

fn parse_op(c: char) -> Ops {
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

    fn parser(tokens: &[Tokens]) -> AST {
        let mut ast: AST = AST::Input; // Initially set to Input to indicate an empty expression
        let mut paren_start: Option<usize> = None;
    
        for (i, token) in tokens.iter().enumerate() {
            match token {
                Tokens::OpenParen => {
                    if paren_start.is_none() {
                        paren_start = Some(i);
                    }
                }
                Tokens::CloseParen => {
                    if let Some(start) = paren_start {
                        let paren_tokens = &tokens[start + 1..i]; // Extract tokens inside parentheses
                        let paren_ast = parser(paren_tokens); // Parse tokens inside parentheses
                        ast = replace_input_node(ast, paren_ast); // Replace Input node with the parsed expression
                        paren_start = None;
                    } else {
                        panic!("Invalid closing parenthesis");
                    }
                }
                Tokens::Combinator(combinator) => {
                    ast = replace_input_node(ast, generate_combinator_structure(combinator.clone()));
                }
                Tokens::Op(op) => {
                    ast = replace_input_node(ast, generate_op_node_structure(op.clone()));
                }
            }
        }
    
        if paren_start.is_some() {
            panic!("Unclosed parenthesis");
        }
    
        ast
    }

fn replace_input_node(ast: AST, input: AST) -> AST {
    match ast {
        AST::Operator(op, left, right) => {
            match *left {
                AST::Input => AST::Operator(op, Box::new(input), right),
                _ => AST::Operator(op, Box::new(replace_input_node(*left, input)), right),
            }
        }
        AST::Combinator(combinator, left, right) => {
            match *left {
                AST::Input => AST::Combinator(combinator, Box::new(input), right),
                _ => AST::Combinator(combinator, Box::new(replace_input_node(*left, input)), right),
            }
        }
        AST::Input => input,
    }
}

fn generate_combinator_structure(combinator: Combinators) -> AST {
    match combinator {
        Combinators::S => AST::Combinator(Combinators::S, Box::new(AST::Input), Box::new(AST::Input)),
        Combinators::K => AST::Combinator(Combinators::K, Box::new(AST::Input), Box::new(AST::Input)),
    }
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
        AST::Combinator(combinator, left, right) => {
            let left_value = eval(*left);
            let right_value = eval(*right);
            match combinator {
                Combinators::S => right_value * left_value + right_value,
                Combinators::K => left_value,
            }
        }
        AST::Input => {
            // Prompt the user for input until a valid integer is entered
            loop {
                let mut input = String::new();
                std::io::stdin().read_line(&mut input).expect("Failed to read input");
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
