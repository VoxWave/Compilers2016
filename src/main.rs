mod file_handling;
mod source_processing;
mod interpreter;

fn main() {
    let mut source = file_handling::get_source_file();
    let mut tokenized = source_processing::scan(source);
    let mut parsed = source_processing::parse(tokenized);
    let mut interprettable = source_processing::sem_analyze(par);
}
