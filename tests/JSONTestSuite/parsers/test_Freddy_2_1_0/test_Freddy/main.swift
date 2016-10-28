//
//  main.swift
//  test_Freddy
//
//  Created by nst on 10/08/16.
//  Copyright © 2016 Nicolas Seriot. All rights reserved.
//

import Foundation

func main() {
    
    guard Process.arguments.count == 2 else {
        let url = NSURL(fileURLWithPath: Process.arguments[0])
        guard let programName = url.lastPathComponent else { exit(1) }
        print("Usage: ./\(programName) file.json")
        exit(1)
    }
    
    let path = Process.arguments[1]
    let url = NSURL.fileURLWithPath(path)
    
    guard let data = NSData(contentsOfURL:url) else {
        print("*** CANNOT READ DATA AT \(url)")
        return
    }
    
    var p = JSONParser(utf8Data: data)
    do {
        try p.parse()
        exit(0)
    } catch {
        exit(1)
    }
}

main()
