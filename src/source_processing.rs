use util::Direction;

enum Token {
	Bracket(Direction),
	CurlyBracket(Direction),
	Identifier(String),
	StringLiteral(String),
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

enum ScanMode {
	Normal, String,
}

fn scan(source: String) -> Vec<Token> {
	let mut scan_mode = ScanMode::Normal;
    let mut processed_strings: Vec<Token> = Vec::new();
    let mut buffer_string = String::new();

    for c in source.chars() {
		match scan_mode {
			ScanMode::Normal => {
				match c {
			        ';' => {
						eval_buffer(&mut buffer_string);
					},
			    }
			}
			ScanMode::String => {

			},
		}
    }
}

fn eval_buffer(buffer_string: &mut String) -> {

}
