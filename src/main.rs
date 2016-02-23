mod file_handling;
mod source_processing;
mod interpreter;

fn main() {
    let mut raw_source = file_handling::get_source_file();
    let mut lines = source_processing::process_raw_source(raw_source);
}
