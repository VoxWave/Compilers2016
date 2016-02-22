fn get_source_file() -> Result<File, String> {

}

fn get_path() -> String {
    //let f = try!(File::open());
    let mut path = String::new();

    for (i, argument) in env::args().enumerate() {
        if i == 1 {
            path = argument.to_string()
        }
    }

    if path.is_empty() {
        panic!("No path to a source file provided.");
        path
    }
}
