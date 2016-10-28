//
//  JSONParsing.swift
//  Freddy
//
//  Created by Matthew D. Mathias on 3/17/15.
//  Copyright © 2015 Big Nerd Ranch. All rights reserved.
//

import Foundation

// MARK: - Deserialize JSON

/// Protocol describing a backend parser that can produce `JSON` from `NSData`.
public protocol JSONParserType {

    /// Creates an instance of `JSON` from `NSData`.
    /// - parameter data: An instance of `NSData` to use to create `JSON`.
    /// - throws: An error that may arise from calling `JSONObjectWithData(_:options:)` on `NSJSONSerialization` with the given data.
    /// - returns: An instance of `JSON`.
    static func createJSONFromData(data: NSData) throws -> JSON

}

extension JSON {

    /// Create `JSON` from UTF-8 `data`. By default, parses using the
    /// Swift-native `JSONParser` backend.
    public init(data: NSData, usingParser parser: JSONParserType.Type = JSONParser.self) throws {
        self = try parser.createJSONFromData(data)
    }

    /// Create `JSON` from UTF-8 `string`.
    public init(jsonString: Swift.String, usingParser parser: JSONParserType.Type = JSONParser.self) throws {
        self = try parser.createJSONFromData((jsonString as NSString).dataUsingEncoding(NSUTF8StringEncoding) ?? NSData())
    }
}

// MARK: - NSJSONSerialization

extension NSJSONSerialization: JSONParserType {

    // MARK: Decode NSData

    /// Use the built-in, Objective-C based JSON parser to create `JSON`.
    /// - parameter data: An instance of `NSData`.
    /// - returns: An instance of `JSON`.
    /// - throws: An error that may arise if the `NSData` cannot be parsed into an object.
    public static func createJSONFromData(data: NSData) throws -> JSON {
        return makeJSON(try NSJSONSerialization.JSONObjectWithData(data, options: []))
    }

    // MARK: Make JSON

    /// Makes a `JSON` object by matching its argument to a case in the `JSON` enum.
    /// - parameter object: The instance of `AnyObject` returned from serializing the JSON.
    /// - returns: An instance of `JSON` matching the JSON given to the function.
    private static func makeJSON(object: AnyObject) -> JSON {
        switch object {
        case let n as NSNumber:
            let numberType = CFNumberGetType(n)
            switch numberType {
            case .CharType:
                return .Bool(n.boolValue)

            case .ShortType, .IntType, .LongType, .CFIndexType, .NSIntegerType, .SInt8Type, .SInt16Type, .SInt32Type:
                return .Int(n.integerValue)

            case .SInt64Type, .LongLongType /* overflows 32-bit Int */:
                #if /* 32-bit arch */ arch(arm) || arch(i386)
                    // Why double, when the Freddy parser would bump to String?
                    //
                    // Returning Double avoids making the type depend on whether you're running
                    // 32-bit or 64-bit code when using the NSJSONSerialization parser.
                    // NSJSONSerialization appears to bump numbers larger than Int.max to Double on
                    // 64-bit platforms but use .SInt64Type on 32-bit platforms.
                    // If we returned a String here, you'd get a String value on 32-bit,
                    // but a Double value on 64-bit. Instead, we return Double.
                    //
                    // This means that, if you switch parsers,
                    // you'll have to switch from .double to .string for pulling out
                    // overflowing values, but if you stick with a single parser,
                    // you at least won't have architecture-dependent lookups!
                    return .Double(n.doubleValue)
                #else
                    return .Int(n.integerValue)
                #endif

            case .Float32Type, .Float64Type, .FloatType, .DoubleType, .CGFloatType:
                return .Double(n.doubleValue)
            }

        case let arr as [AnyObject]:
            return makeJSONArray(arr)

        case let dict as [Swift.String: AnyObject]:
            return makeJSONDictionary(dict)

        case let s as Swift.String:
            return .String(s)

        default:
            return .Null
        }
    }

    // MARK: Make a JSON Array

    /// Makes a `JSON` array from the object passed in.
    /// - parameter jsonArray: The array to transform into a `JSON`.
    /// - returns: An instance of `JSON` matching the array.
    private static func makeJSONArray(jsonArray: [AnyObject]) -> JSON {
        return .Array(jsonArray.map(makeJSON))
    }

    // MARK: Make a JSON Dictionary

    /// Makes a `JSON` dictionary from the Cocoa dictionary passed in.
    /// - parameter jsonDict: The dictionary to transform into `JSON`.
    /// - returns: An instance of `JSON` matching the dictionary.
    private static func makeJSONDictionary(jsonDict: [Swift.String: AnyObject]) -> JSON {
        return JSON(jsonDict.lazy.map { (key, value) in
            (key, makeJSON(value))
        })
    }

}
