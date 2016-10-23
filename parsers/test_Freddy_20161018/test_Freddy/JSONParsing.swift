//
//  JSONParsing.swift
//  Freddy
//
//  Created by Matthew D. Mathias on 3/17/15.
//  Copyright © 2015 Big Nerd Ranch. All rights reserved.
//

import Foundation

// MARK: - Deserialize JSON

/// Protocol describing a backend parser that can produce `JSON` from `Data`.
public protocol JSONParserType {

    /// Creates an instance of `JSON` from `Data`.
    /// - parameter data: An instance of `Data` to use to create `JSON`.
    /// - throws: An error that may arise from calling `JSONObjectWithData(_:options:)` on `NSJSONSerialization` with the given data.
    /// - returns: An instance of `JSON`.
    static func createJSON(from data: Data) throws -> JSON

}

extension JSON {

    /// Create `JSON` from UTF-8 `data`. By default, parses using the
    /// Swift-native `JSONParser` backend.
    public init(data: Data, usingParser parser: JSONParserType.Type = JSONParser.self) throws {
        self = try parser.createJSON(from: data)
    }

    /// Create `JSON` from UTF-8 `string`.
    public init(jsonString: Swift.String, usingParser parser: JSONParserType.Type = JSONParser.self) throws {
        self = try parser.createJSON(from: jsonString.data(using: Swift.String.Encoding.utf8) ?? Data())
    }
}

// MARK: - NSJSONSerialization

extension JSONSerialization: JSONParserType {

    // MARK: Decode Data

    /// Use the built-in, Objective-C based JSON parser to create `JSON`.
    /// - parameter data: An instance of `Data`.
    /// - returns: An instance of `JSON`.
    /// - throws: An error that may arise if the `Data` cannot be parsed into an object.
    public static func createJSON(from data: Data) throws -> JSON {
        return makeJSON(with: try JSONSerialization.jsonObject(with: data, options: []))
    }

    // MARK: Make JSON

    /// Makes a `JSON` object by matching its argument to a case in the `JSON` enum.
    /// - parameter object: The instance of `Any` returned from serializing the JSON.
    /// - returns: An instance of `JSON` matching the JSON given to the function.
    private static func makeJSON(with object: Any) -> JSON {
        switch object {
        case let n as NSNumber:
            let numberType = CFNumberGetType(n)
            switch numberType {
            case .charType:
                return .bool(n.boolValue)

            case .shortType, .intType, .longType, .cfIndexType, .nsIntegerType, .sInt8Type, .sInt16Type, .sInt32Type:
                return .int(n.intValue)

            case .sInt64Type, .longLongType /* overflows 32-bit Int */:
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
                    return .double(n.doubleValue)
                #else
                    return .int(n.intValue)
                #endif

            case .float32Type, .float64Type, .floatType, .doubleType, .cgFloatType:
                return .double(n.doubleValue)
            }

        case let arr as [Any]:
            return makeJSONArray(arr)

        case let dict as [Swift.String: Any]:
            return makeJSONDictionary(dict)

        case let s as Swift.String:
            return .string(s)

        default:
            return .null
        }
    }

    // MARK: Make a JSON Array

    /// Makes a `JSON` array from the object passed in.
    /// - parameter jsonArray: The array to transform into a `JSON`.
    /// - returns: An instance of `JSON` matching the array.
    private static func makeJSONArray(_ jsonArray: [Any]) -> JSON {
        return .array(jsonArray.map(makeJSON))
    }

    // MARK: Make a JSON Dictionary

    /// Makes a `JSON` dictionary from the Cocoa dictionary passed in.
    /// - parameter jsonDict: The dictionary to transform into `JSON`.
    /// - returns: An instance of `JSON` matching the dictionary.
    private static func makeJSONDictionary(_ jsonDict: [Swift.String: Any]) -> JSON {
        return JSON(jsonDict.lazy.map { (key, value) in
            (key, makeJSON(with: value))
        })
    }

}
