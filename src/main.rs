mod util;
mod file_handling;
mod source_processing;
mod scanner;
mod interpreter;

use scanner::Scanner;

fn main() {
    let mut scanner = Scanner::new(file_handling::get_source_text());
    let mut tokenized = scanner.scan();
    let mut parsed = source_processing::parse(tokenized);
    let mut interprettable = source_processing::sem_analyze(par);
}
