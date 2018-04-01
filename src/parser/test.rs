use super::{parse, Statement};

#[test]
fn example_program_2() {
    let source = r#" var nTimes : int := 0;
 print "How many times?";
 read nTimes;
 var x : int;
 for x in 0..nTimes-1 do
  print x;
  print " : Hello, World!\n";
 end for;
 assert (x = nTimes);"#;
    let mut scanner = Scanner::new();
    let mut tokens = Vec::new();
    scanner.scan(source, &mut tokens);
    let mut statements = Vec::new();
    parse(&mut tokens, &mut statements);
}