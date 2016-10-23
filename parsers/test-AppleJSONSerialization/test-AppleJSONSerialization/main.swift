//
//  main.swift
//  test-AppleJSONSerialization
//
//  Created by nst on 26/08/16.
//  Copyright © 2016 Nicolas Seriot. All rights reserved.
//

import Foundation

func main() {
    
    guard ProcessInfo.processInfo.arguments.count == 2 else {
        let url = NSURL(fileURLWithPath: ProcessInfo.processInfo.arguments[0])
        guard let programName = url.lastPathComponent else { exit(1) }
        print("Usage: ./\(programName) file.json")
        exit(1)
    }
    
    let path = ProcessInfo.processInfo.arguments[1]
    let url = NSURL.fileURL(withPath:path)
    
    do {
        let data = try Data(contentsOf:url)
        
        let _ = try JSONSerialization.jsonObject(with: data, options: [.allowFragments])

        exit(0)
    } catch {
        exit(1)
    }
}

main()
