
fn eval(ast: AST, last: Option<i32>) -> i32 {
    match ast {
        AST::Operator(op, left, right) => {
            let left_value = eval(*left);
            let right_value = eval(*right);

            match op {
                Ops::Mul => left_value * right_value,
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
                    Ok(value) => {
                        unsafe { LAST = Some(value) };
                        break value
                    },
                    Err(_) => println!("Invalid input! Please enter a valid integer."),
                }
            }
        }
        AST::Dup => {
            unsafe { LAST.unwrap() }
        }
    }
}
