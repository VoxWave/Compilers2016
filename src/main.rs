use std::env;
use std::fs::File;

mod file_handling;

fn main() {
    let mut file = file_handling::get_source_file();
}
