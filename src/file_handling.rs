use std::env;
use std::fs::File;

fn get_source_text() -> String {
    let path = get_path();
    let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", display,
                                                   Error::description(&why)),
        Ok(file) => file,
    };
    let mut source_text = String::new();
    // I should read the file to a string until a ';' character comes up and then start a new string
    // and store all the resulting strings in a Vec or some other data structure. I don't know how
    // to do that in rust yet and that's why I just read the whole source file into a string and
    // chop up the string into smaller strings afterwards.
    match file.read_to_string(&mut source_text) {
        Err(why) => panic!("couldn't read {}: {}", display,
                                                   Error::description(&why)),
        Ok(_) => {}
    }
    source_text
}

fn get_path() -> Path {
    //let f = try!(File::open());
    let mut path = String::new();

    for (i, argument) in env::args().enumerate() {
        if i == 1 {
            path = argument.to_string()
        }
    }

    if path.is_empty() {
        panic!("No path to a source file provided.");
    }
    path
}
