mod lexer;

use lexer::*;

fn main() {
    loop {
        let mut input = String::new();
        while !input.contains(";") {
            let mut next_line = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            input.push_str(next_line.as_str())
        }
        println!("lexing: {:?}", lex_fn(input.as_str()))
    }
}
