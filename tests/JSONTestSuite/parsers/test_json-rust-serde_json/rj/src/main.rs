extern crate serde_json;
use std::fs::File;
use std::io::Read;
use std::env;

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
    //println!("{}", s);

    match f.read_to_string(&mut s) {
        Err(_) => std::process::exit(1),
        Ok(_) => println!("{}", s),
    }

    match serde_json::from_str::<serde_json::Value>(&s) {
        Ok(_) => std::process::exit(0),
        Err(_) => std::process::exit(1)
    };
}
