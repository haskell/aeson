//
//  main.swift
//  test_Freddy
//
//  Created by nst on 10/08/16.
//  Copyright © 2016 Nicolas Seriot. All rights reserved.
//

import Foundation

func main() {
    
    guard ProcessInfo.processInfo.arguments.count == 2 else {
        let url = URL(fileURLWithPath: ProcessInfo.processInfo.arguments[0])
        let programName = url.lastPathComponent
        print("Usage: ./\(programName) file.json")
        exit(1)
    }

    let path = ProcessInfo.processInfo.arguments[1]
    let url = NSURL.fileURL(withPath:path)
    
    do {
        let data = try Data(contentsOf:url)
        do {
            let o = try JSONParser.parse(utf8: data)
            exit(0)
        } catch {
            exit(1)
        }

    } catch let e {
        print("*** CANNOT READ DATA AT \(url)")
        print(e)
        exit(1)
    }
    
}

main()
