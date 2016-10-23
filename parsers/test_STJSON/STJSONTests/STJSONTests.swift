//
//  STJSONTests.swift
//  STJSONTests
//
//  Created by Nicolas Seriot on 17.10.16.
//  Copyright © 2016 ch.seriot. All rights reserved.
//

import XCTest

class STJSONTests: XCTestCase {
    
    func parseData(data:Data) -> Any? {
        
        var p = STJSONParser(data: data)
        do {
            //try data.write(to: URL(fileURLWithPath: "/Users/nst/Desktop/data.json"))
            return try p.parse()
        } catch let e {
            print(e)
            return nil
        }
    }
    
    func parseString(_ s:String) -> Any? {
        guard let data = s.data(using: String.Encoding.utf8) else { XCTFail(); return nil }
        
        return parseData(data:data)
    }
    
    func testCases() {
        let dir = "/Users/nst/Projects/dropbox/JSON/test_cases/"

        guard let enumerator = FileManager.default.enumerator(atPath: dir) else {
            // please checkout tests from https://github.com/nst/JSONTestSuite
            // and setup the path in this method
            XCTFail()
            return
        }
        
        for filename in enumerator where (filename as! NSString).pathExtension == "json" {
            
            guard let s = filename as? String else { continue }
            
            let url = URL(fileURLWithPath: dir+s)
            
            print("*** testing \(s)")
            
            do {
                let data = try Data(contentsOf: url)
                
                let o = parseData(data:data)
                
                if s.hasPrefix("n_") {
                    if o != nil {
                        print("STJSON\tSHOULD_HAVE_FAILED\t\(filename)")
                    }
                    XCTAssertNil(o)
                } else if s.hasPrefix("y_") {
                    if o == nil {
                        print("STJSON\tSHOULD_HAVE_PASSED\t\(filename)")
                    }
                    XCTAssertNotNil(o)
                } else if o == nil {
                    print("STJSON\tIMPLEMENTATION_FAIL\t\(filename)")
                } else {
                    print("STJSON\tIMPLEMENTATION_PASS\t\(filename)")
                }
            } catch let e {
                print(e)
                XCTFail()
            }
        }
    }
    
    func testReadDouble() {
        XCTAssertEqual(2.0, STJSONParser(data:Data()).myDouble("2.0"))
    }
    
    func testReadDoubleWithExponent() {
        XCTAssertEqual(100.0, STJSONParser(data:Data()).myDouble("10.0e1"))
    }
    
    func testReadExponent() {
        
        let i = Int(Double("20e2")!)
        print("***", i)
        
        let o = parseString("[20e2]") as! [Double]
        XCTAssertEqual([2000], o)
    }
}
