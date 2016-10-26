//
//  main.swift
//  test_PMJSON
//
//  Created by nst on 26.10.16.
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
            let json = try JSON.decode(data)
            
            guard json != nil else {
                exit(1)
            }
            
            exit(0)
        } catch let e {
            print(e)
            exit(1)
        }
    } catch let e {
        print("*** CANNOT READ DATA AT \(url)")
        print(e)
        exit(1)
    }
}

main()
