extern crate char_stream;
extern crate num;
extern crate num_bigint;
extern crate rayon;

use std::sync::mpsc::channel;
use std::sync::mpsc::Sender;
use std::sync::mpsc::Receiver;

use std::process::abort;

use rayon::ThreadPoolBuilder;

pub mod file_handling;
pub mod scanner;
pub mod parser;
pub mod interpreter;
pub mod util;

use scanner::Scanner;
use interpreter::Interpreter;

//use scanner::Scanner;

fn main() {
    let pool = ThreadPoolBuilder::new().num_threads(3).build().unwrap();
    let (mut token_sink, mut token_source) = channel();
    let (mut statement_sink, mut statement_source) = channel();
    let (mut kill_switch, mut kill_signal) = channel();

    pool.spawn(move || {
        let mut scanner = Scanner::new();
        scanner.scan(&file_handling::get_source_text(), &mut token_sink);
    });
    pool.spawn(move || {
        parser::parse(&mut token_source, &mut statement_sink);
    });
    pool.spawn(move || {
        let mut interpreter = Interpreter::new();
        interpreter.interpret(&mut statement_source);
        kill_switch.send(true);
    });
    kill_signal.recv();
}
