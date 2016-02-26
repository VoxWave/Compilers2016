use util::Direction;

enum Token {
	Bracket(Direction),
	CurlyBracket(Direction),
	Semicolon,
	Colon
	Var,
	For,
	End,
	In,
	Do,
	Read,
	Print,
	Int,
	String,
	Bool,
	Assert,
}

fn scan(raw: String) -> Vec<Token> {

    let mut processed_strings = Vec::new();
    let mut buffer_string = String::new();

    for c in raw.chars() {
        match c {
            ';' => {},
        }
    }
}
