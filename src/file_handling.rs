//     mini-pl compiler.
//     Copyright (C) 2018  Victor Bankowski

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

use std::env;
use std::io::Read;
use std::fs::File;
use std::path::PathBuf;
use std::error::Error;

pub fn get_source_text() -> String {
    let path = get_path();
    let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", display, Error::description(&why)),
        Ok(file) => file,
    };
    let mut source_text = String::new();
    // I should read the file to a string until a ';' character comes up and then start a new string
    // and store all the resulting strings in a Vec or some other data structure. I don't know how
    // to do that in rust yet and that's why I just read the whole source file into a string and
    // chop up the string into smaller strings afterwards.
    match file.read_to_string(&mut source_text) {
        Err(why) => panic!("couldn't read {}: {}", display, Error::description(&why)),
        Ok(_) => {}
    }
    source_text
}

fn get_path() -> PathBuf {
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
    PathBuf::from(path)
}
