use std::fs::File;
use std::io::Read;
use std::env;
//use std::process;

#[macro_use]
extern crate json;

fn main() {
    println!("Hello, world!");

    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} file.json", args[0]);
	    std::process::exit(1);
    }
    
    let ref path = args[1];
    let mut s = String::new();
    let mut f = File::open(path).expect("Unable to open file");
    //f.read_to_string(&mut s).expect("Unable to read string");

    match f.read_to_string(&mut s) {
        Err(_) => std::process::exit(1),
        Ok(_) => println!("{}", s),
    }
    
    // println!("{}", s);
    // println!("{:?}", t!(s));
    // let bytes: &[u8] = owned_hello.as_bytes();
	
	//let o = json::parse(&s);
    //println!("{:?}", o);
    
    match json::parse(&s) {
	    Err(_) => std::process::exit(1),
	    _ => std::process::exit(0),
    }

    //println!("{}", s);

}
