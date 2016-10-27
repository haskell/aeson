//
//  DecimalNumber.swift
//  PMJSON
//
//  Created by Kevin Ballard on 2/8/16.
//  Copyright © 2016 Postmates.
//
//  Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
//  http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
//  <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
//  option. This file may not be copied, modified, or distributed
//  except according to those terms.
//

#if os(iOS) || os(OSX) || os(tvOS) || os(watchOS)
    
    import Foundation
    
    // MARK: Basic accessors
    
    public extension JSON {
        /// Returns the receiver as an `NSDecimalNumber` if possible.
        /// - Returns: An `NSDecimalNumber` if the receiver is `.int64` or `.double`, or is a `.string`
        ///   that contains a valid decimal number representation, otherwise `nil`.
        /// - Note: Whitespace is not allowed in the string representation.
        var asDecimalNumber: NSDecimalNumber? {
            switch self {
            case .int64(let i): return NSDecimalNumber(value: i)
            case .double(let d): return NSDecimalNumber(value: d)
            case .string(let s) where !s.isEmpty:
                // NSDecimalNumber(string:) doesn't tell us if the number was valid.
                // We could check for NaN, but that still doesn't tell us if there's anything left in the string.
                // I'm pretty sure it uses NSScanner.scanDecimal() internally, so we'll just use that instead.
                let scanner = Scanner(string: s)
                scanner.charactersToBeSkipped = nil
                var decimal = Decimal()
                if scanner.scanDecimal(&decimal) && scanner.isAtEnd {
                    return NSDecimalNumber(decimal: decimal)
                }
                return nil
            default: return nil
            }
        }
        
        /// Returns the receiver as an `NSDecimalNumber` if it is `.int64` or `.double`.
        /// - Returns: An `NSDecimalNumber`.
        /// - Throws: `JSONError` if the receiver is not an `.int64` or a `.double`.
        func getDecimalNumber() throws -> NSDecimalNumber {
            switch self {
            case .int64(let i): return NSDecimalNumber(value: i)
            case .double(let d): return NSDecimalNumber(value: d)
            default: throw JSONError.missingOrInvalidType(path: nil, expected: .required(.number), actual: .forValue(self))
            }
        }
        
        /// Returns the receiver as an `NSDecimalNumber` if it is `.int64` or `.double`.
        /// - Returns: An `NSDecimalNumber`, or `nil` if the receivre is `null`.
        /// - Throws: `JSONError` if the receiver is not an `.int64` or a `.double`.
        func getDecimalNumberOrNil() throws -> NSDecimalNumber? {
            switch self {
            case .int64(let i): return NSDecimalNumber(value: i)
            case .double(let d): return NSDecimalNumber(value: d)
            case .null: return nil
            default: throw JSONError.missingOrInvalidType(path: nil, expected: .required(.number), actual: .forValue(self))
            }
        }
        
        /// Returns the receiver as an `NSDecimalNumber` if possible.
        /// - Returns: An `NSDecimalNumber` if the receiver is `.int64` or `.double`, or is a `.string`
        ///   that contains a valid decimal number representation.
        /// - Throws: `JSONError` if the receiver is the wrong type, or is a `.string` that does not contain
        ///   a valid decimal number representation.
        /// - Note: Whitespace is not allowed in the string representation.
        func toDecimalNumber() throws -> NSDecimalNumber {
            guard let value = asDecimalNumber else {
                throw JSONError.missingOrInvalidType(path: nil, expected: .required(.number), actual: .forValue(self))
            }
            return value
        }
        
        /// Returns the receiver as an `NSDecimalNumber` if possible.
        /// - Returns: An `NSDecimalNumber` if the receiver is `.int64` or `.double`, or is a `.string`
        ///   that contains a valid decimal number representation, or `nil` if the receiver is `null`.
        /// - Throws: `JSONError` if the receiver is the wrong type, or is a `.string` that does not contain
        ///   a valid decimal number representation.
        /// - Note: Whitespace is not allowed in the string representation.
        func toDecimalNumberOrNil() throws -> NSDecimalNumber? {
            if let value = asDecimalNumber { return value }
            else if isNull { return nil }
            else { throw JSONError.missingOrInvalidType(path: nil, expected: .optional(.number), actual: .forValue(self)) }
        }
    }
    
    // MARK: - Keyed accessors
    
    public extension JSON {
        /// Subscripts the receiver with `key` and returns the result as an `NSDecimalNumber`.
        /// - Parameter key: The key that's used to subscript the receiver.
        /// - Returns: An `NSDecimalNumber`.
        /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type, or if
        ///   the receiver is not an object.
        func getDecimalNumber(_ key: String) throws -> NSDecimalNumber {
            let dict = try getObject()
            let value = try getRequired(dict, key: key, type: .number)
            return try scoped(key) { try value.getDecimalNumber() }
        }
        
        /// Subscripts the receiver with `key` and returns the result as an `NSDecimalNumber`.
        /// - Parameter key: The key that's used to subscript the receiver.
        /// - Returns: An `NSDecimalNumber`, or `nil` if the key doesn't exist or the value is `null`.
        /// - Throws: `JSONError` if the value is the wrong type, or if the receiver is
        ///   not an object.
        func getDecimalNumberOrNil(_ key: String) throws -> NSDecimalNumber? {
            let dict = try getObject()
            guard let value = dict[key] else { return nil }
            return try scoped(key) { try value.getDecimalNumberOrNil() }
        }
        
        /// Subscripts the receiver with `key` and returns the result as an `NSDecimalNumber`.
        /// - Parameter key: The key that's used to subscript the receiver.
        /// - Returns: An `NSDecimalNumber`.
        /// - Throws: `JSONError` if the key doesn't exist or the value is `null`, a boolean, an object,
        ///   an array, or a string that cannot be coerced to a decimal number, or if the
        ///   receiver is not an object.
        func toDecimalNumber(_ key: String) throws -> NSDecimalNumber {
            let dict = try getObject()
            let value = try getRequired(dict, key: key, type: .number)
            return try scoped(key) { try value.toDecimalNumber() }
        }
        
        /// Subscripts the receiver with `key` and returns the result as an `NSDecimalNumber`.
        /// - Parameter key: The key that's used to subscript the receiver.
        /// - Returns: An `NSDecimalNumber`, or `nil` if the key doesn't exist or the value is `null`.
        /// - Throws: `JSONError` if the value is a boolean, an object, an array, or a string that
        ///   cannot be coerced to a decimal number, or if the receiver is not an object.
        func toDecimalNumberOrNil(_ key: String) throws -> NSDecimalNumber? {
            let dict = try getObject()
            guard let value = dict[key] else { return nil }
            return try scoped(key) { try value.toDecimalNumberOrNil() }
        }
    }
    
    // MARK: - Indexed accessors
    
    public extension JSON {
        /// Subscripts the receiver with `index` and returns the result as an `NSDecimalNumber`.
        /// - Parameter index: The index that's used to subscript the receiver.
        /// - Returns: An `NSDecimalNumber`.
        /// - Throws: `JSONError` if the index is out of bounds or the value is the wrong type, or if
        ///   the receiver is not an array.
        func getDecimalNumber(_ index: Int) throws -> NSDecimalNumber {
            let array = try getArray()
            let value = try getRequired(array, index: index, type: .number)
            return try scoped(index) { try value.getDecimalNumber() }
        }
        
        /// Subscripts the receiver with `index` and returns the result as an `NSDecimalNumber`.
        /// - Parameter index: The index that's used to subscript the receiver.
        /// - Returns: An `NSDecimalNumber`, or `nil` if the index is out of bounds or the value is `null`.
        /// - Throws: `JSONError` if the value is the wrong type, or if the receiver is not an array.
        func getDecimalNumberOrNil(_ index: Int) throws -> NSDecimalNumber? {
            let array = try getArray()
            guard let value = array[safe: index] else { return nil }
            return try scoped(index) { try value.getDecimalNumberOrNil() }
        }
        
        /// Subscripts the receiver with `index` and returns the result as an `NSDecimalNumber`.
        /// - Parameter index: The index that's used to subscript the receiver.
        /// - Returns: An `NSDecimalNumber`.
        /// - Throws: `JSONError` if the index is out of bounds or the value is `null`, a boolean,
        ///   an object, an array, or a string that cannot be coerced to a decimal number, or
        ///   if the receiver is not an array.
        func toDecimalNumber(_ index: Int) throws -> NSDecimalNumber {
            let array = try getArray()
            let value = try getRequired(array, index: index, type: .number)
            return try scoped(index) { try value.toDecimalNumber() }
        }
        
        /// Subscripts the receiver with `index` and returns the result as an `NSDecimalNumber`.
        /// - Parameter index: The index that's used to subscript the receiver.
        /// - Returns: An `NSDecimalNumber`, or `nil` if the index is out of bounds or the value is `null`.
        /// - Throws: `JSONError` if the value is a boolean, an object, an array, or a string that
        ///   cannot be coerced to a decimal number, or if the receiver is not an array.
        func toDecimalNumberOrNil(_ index: Int) throws -> NSDecimalNumber? {
            let array = try getArray()
            guard let value = array[safe: index] else { return nil }
            return try scoped(index) { try value.toDecimalNumberOrNil() }
        }
    }
    
    // MARK: -
    
    public extension JSONObject {
        /// Subscripts the receiver with `key` and returns the result as an `NSDecimalNumber`.
        /// - Parameter key: The key that's used to subscript the receiver.
        /// - Returns: An `NSDecimalNumber`.
        /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type, or if
        ///   the receiver is not an object.
        func getDecimalNumber(_ key: String) throws -> NSDecimalNumber {
            let value = try getRequired(self, key: key, type: .number)
            return try scoped(key) { try value.getDecimalNumber() }
        }
        
        /// Subscripts the receiver with `key` and returns the result as an `NSDecimalNumber`.
        /// - Parameter key: The key that's used to subscript the receiver.
        /// - Returns: An `NSDecimalNumber`, or `nil` if the key doesn't exist or the value is `null`.
        /// - Throws: `JSONError` if the value is the wrong type, or if the receiver is
        ///   not an object.
        func getDecimalNumberOrNil(_ key: String) throws -> NSDecimalNumber? {
            guard let value = self[key] else { return nil }
            return try scoped(key) { try value.getDecimalNumberOrNil() }
        }
        
        /// Subscripts the receiver with `key` and returns the result as an `NSDecimalNumber`.
        /// - Parameter key: The key that's used to subscript the receiver.
        /// - Returns: An `NSDecimalNumber`.
        /// - Throws: `JSONError` if the key doesn't exist or the value is `null`, a boolean, an object,
        ///   an array, or a string that cannot be coerced to a decimal number, or if the
        ///   receiver is not an object.
        func toDecimalNumber(_ key: String) throws -> NSDecimalNumber {
            let value = try getRequired(self, key: key, type: .number)
            return try scoped(key) { try value.toDecimalNumber() }
        }
        
        /// Subscripts the receiver with `key` and returns the result as an `NSDecimalNumber`.
        /// - Parameter key: The key that's used to subscript the receiver.
        /// - Returns: An `NSDecimalNumber`, or `nil` if the key doesn't exist or the value is `null`.
        /// - Throws: `JSONError` if the value is a boolean, an object, an array, or a string that
        ///   cannot be coerced to a decimal number, or if the receiver is not an object.
        func toDecimalNumberOrNil(_ key: String) throws -> NSDecimalNumber? {
            guard let value = self[key] else { return nil }
            return try scoped(key) { try value.toDecimalNumberOrNil() }
        }
    }
    
#endif // os(iOS) || os(OSX) || os(tvOS) || os(watchOS)
