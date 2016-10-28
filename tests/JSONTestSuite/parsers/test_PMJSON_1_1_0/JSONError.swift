//
//  JSONError.swift
//  PMJSON
//
//  Created by Kevin Ballard on 11/9/15.
//  Copyright © 2016 Postmates.
//
//  Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
//  http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
//  <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
//  option. This file may not be copied, modified, or distributed
//  except according to those terms.
//

// MARK: JSONError

/// Errors thrown by the JSON `get*` or `to*` accessor families.
public enum JSONError: Error, CustomStringConvertible {
    /// Thrown when a given path is missing or has the wrong type.
    /// - Parameter path: The path of the key that caused the error.
    /// - Parameter expected: The type that was expected at this path.
    /// - Parameter actual: The type of the value found at the path, or `nil` if there was no value.
    case missingOrInvalidType(path: String?, expected: ExpectedType, actual: JSONType?)
    /// Thrown when an integral value is coerced to a smaller type (e.g. `Int64` to `Int`) and the
    /// value doesn't fit in the smaller type.
    /// - Parameter path: The path of the value that cuased the error.
    /// - Parameter value: The actual value at that path.
    /// - Parameter expected: The type that the value doesn't fit in, e.g. `Int.self`.
    case outOfRangeInt64(path: String?, value: Int64, expected: Any.Type)
    /// Thrown when a floating-point value is coerced to a smaller type (e.g. `Double` to `Int`)
    /// and the value doesn't fit in the smaller type.
    /// - Parameter path: The path of the value that cuased the error.
    /// - Parameter value: The actual value at that path.
    /// - Parameter expected: The type that the value doesn't fit in, e.g. `Int.self`.
    case outOfRangeDouble(path: String?, value: Double, expected: Any.Type)
    
    public var description: String {
        switch self {
        case let .missingOrInvalidType(path, expected, actual): return "\(path.map({"\($0): "}) ?? "")expected \(expected), found \(actual?.description ?? "missing value")"
        case let .outOfRangeInt64(path, value, expected): return "\(path.map({"\($0): "}) ?? "")value \(value) cannot be coerced to type \(expected)"
        case let .outOfRangeDouble(path, value, expected): return "\(path.map({"\($0): "}) ?? "")value \(value) cannot be coerced to type \(expected)"
        }
    }
    
    fileprivate func withPrefix(_ prefix: String) -> JSONError {
        func prefixPath(_ path: String?, with prefix: String) -> String {
            guard let path = path, !path.isEmpty else { return prefix }
            if path.unicodeScalars.first == "[" {
                return prefix + path
            } else {
                return "\(prefix).\(path)"
            }
        }
        switch self {
        case let .missingOrInvalidType(path, expected, actual):
            return .missingOrInvalidType(path: prefixPath(path, with: prefix), expected: expected, actual: actual)
        case let .outOfRangeInt64(path, value, expected):
            return .outOfRangeInt64(path: prefixPath(path, with: prefix), value: value, expected: expected)
        case let .outOfRangeDouble(path, value, expected):
            return .outOfRangeDouble(path: prefixPath(path, with: prefix), value: value, expected: expected)
        }
    }
    
    public enum ExpectedType: CustomStringConvertible {
        case required(JSONType)
        case optional(JSONType)
        
        public var description: String {
            switch self {
            case .required(let type): return type.description
            case .optional(let type): return "\(type) or null"
            }
        }
    }
    
    public enum JSONType: String, CustomStringConvertible {
        case null = "null"
        case bool = "bool"
        case string = "string"
        case number = "number"
        case object = "object"
        case array = "array"
        
        internal static func forValue(_ value: JSON) -> JSONType {
            switch value {
            case .null: return .null
            case .bool: return .bool
            case .string: return .string
            case .int64, .double: return .number
            case .object: return .object
            case .array: return .array
            }
        }
        
        public var description: String {
            return rawValue
        }
    }
}

// MARK: - Basic accessors
public extension JSON {
    /// Returns the bool value if the receiver is a bool.
    /// - Returns: A `Bool` value.
    /// - Throws: `JSONError` if the receiver is the wrong type.
    func getBool() throws -> Bool {
        guard let b = self.bool else { throw JSONError.missingOrInvalidType(path: nil, expected: .required(.bool), actual: .forValue(self)) }
        return b
    }
    
    /// Returns the bool value if the receiver is a bool.
    /// - Returns: A `Bool` value, or `nil` if the receiver is `null`.
    /// - Throws: `JSONError` if the receiver is the wrong type.
    func getBoolOrNil() throws -> Bool? {
        if let b = self.bool { return b }
        else if isNull { return nil }
        else { throw JSONError.missingOrInvalidType(path: nil, expected: .optional(.bool), actual: .forValue(self)) }
    }
    
    /// Returns the string value if the receiver is a string.
    /// - Returns: A `String` value.
    /// - Throws: `JSONError` if the receiver is the wrong type.
    func getString() throws -> String {
        guard let str = self.string else { throw JSONError.missingOrInvalidType(path: nil, expected: .required(.string), actual: .forValue(self)) }
        return str
    }
    
    /// Returns the string value if the receiver is a string.
    /// - Returns: A `String` value, or `nil` if the receiver is `null`.
    /// - Throws: `JSONError` if the receiver is the wrong type.
    func getStringOrNil() throws -> String? {
        if let str = self.string { return str }
        else if isNull { return nil }
        else { throw JSONError.missingOrInvalidType(path: nil, expected: .optional(.string), actual: .forValue(self)) }
    }
    
    /// Returns the 64-bit integral value if the receiver is a number.
    /// - Returns: An `Int64` value.
    /// - Throws: `JSONError` if the receiver is the wrong type.
    func getInt64() throws -> Int64 {
        guard let val = self.int64 else { throw JSONError.missingOrInvalidType(path: nil, expected: .required(.number), actual: .forValue(self)) }
        return val
    }
    
    /// Returns the 64-bit integral value value if the receiver is a number.
    /// - Returns: An `Int64` value, or `nil` if the receiver is `null`.
    /// - Throws: `JSONError` if the receiver is the wrong type.
    func getInt64OrNil() throws -> Int64? {
        if let val = self.int64 { return val }
        else if isNull { return nil }
        else { throw JSONError.missingOrInvalidType(path: nil, expected: .optional(.number), actual: .forValue(self)) }
    }
    
    /// Returns the integral value if the receiver is a number.
    /// - Returns: An `Int` value.
    /// - Throws: `JSONError` if the receiver is the wrong type, or if the 64-bit integral value
    ///   is too large to fit in an `Int`.
    func getInt() throws -> Int {
        guard let val = self.int64 else { throw JSONError.missingOrInvalidType(path: nil, expected: .required(.number), actual: .forValue(self)) }
        let truncated = Int(truncatingBitPattern: val)
        guard Int64(truncated) == val else { throw JSONError.outOfRangeInt64(path: nil, value: val, expected: Int.self) }
        return truncated
    }
    
    /// Returns the integral value if the receiver is a number.
    /// - Returns: An `Int` value, or `nil` if the receiver is `null`.
    /// - Throws: `JSONError` if the receiver is the wrong type, or if the 64-bit integral value
    ///   is too large to fit in an `Int`.
    func getIntOrNil() throws -> Int? {
        if let val = self.int64 {
            let truncated = Int(truncatingBitPattern: val)
            guard Int64(truncated) == val else { throw JSONError.outOfRangeInt64(path: nil, value: val, expected: Int.self) }
            return truncated
        } else if isNull { return nil }
        else { throw JSONError.missingOrInvalidType(path: nil, expected: .optional(.number), actual: .forValue(self)) }
    }
    
    /// Returns the double value if the receiver is a number.
    /// - Returns: A `Double` value.
    /// - Throws: `JSONError` if the receiver is the wrong type.
    func getDouble() throws -> Double {
        guard let val = self.double else { throw JSONError.missingOrInvalidType(path: nil, expected: .required(.number), actual: .forValue(self)) }
        return val
    }
    
    /// Returns the double value if the receiver is a number.
    /// - Returns: A `Double` value, or `nil` if the receiver is `null`.
    /// - Throws: `JSONError` if the receiver is the wrong type.
    func getDoubleOrNil() throws -> Double? {
        if let val = self.double { return val }
        else if isNull { return nil }
        else { throw JSONError.missingOrInvalidType(path: nil, expected: .optional(.number), actual: .forValue(self)) }
    }
    
    /// Returns the object value if the receiver is an object.
    /// - Returns: An object value.
    /// - Throws: `JSONError` if the receiver is the wrong type.
    func getObject() throws -> JSONObject {
        guard let dict = self.object else { throw JSONError.missingOrInvalidType(path: nil, expected: .required(.object), actual: .forValue(self)) }
        return dict
    }
    
    /// Returns the object value if the receiver is an object.
    /// - Returns: An object value, or `nil` if the receiver is `null`.
    /// - Throws: `JSONError` if the receiver is the wrong type.
    func getObjectOrNil() throws -> JSONObject? {
        if let dict = self.object { return dict }
        else if isNull { return nil }
        else { throw JSONError.missingOrInvalidType(path: nil, expected: .optional(.object), actual: .forValue(self)) }
    }
    
    /// Returns the array value if the receiver is an array.
    /// - Returns: An array value.
    /// - Throws: `JSONError` if the receiver is the wrong type.
    func getArray() throws -> JSONArray {
        guard let ary = self.array else { throw JSONError.missingOrInvalidType(path: nil, expected: .required(.array), actual: .forValue(self)) }
        return ary
    }
    
    /// Returns the array value if the receiver is an array.
    /// - Returns: An array value, or `nil` if the receiver is `null`.
    /// - Throws: `JSONError` if the receiver is the wrong type.
    func getArrayOrNil() throws -> JSONArray? {
        if let ary = self.array { return ary }
        else if isNull { return nil }
        else { throw JSONError.missingOrInvalidType(path: nil, expected: .optional(.array), actual: .forValue(self)) }
    }
}

public extension JSON {
    /// Returns the receiver coerced to a string value.
    /// - Returns: A `String` value.
    /// - Throws: `JSONError` if the receiver is an object or array.
    func toString() throws -> String {
        return try toStringMaybeNil(.required(.string)) ?? "null"
    }
    
    /// Returns the receiver coerced to a string value.
    /// - Returns: A `String` value, or `nil` if the receiver is `null`.
    /// - Throws: `JSONError` if the receiver is an object or array.
    func toStringOrNil() throws -> String? {
        return try toStringMaybeNil(.optional(.string))
    }
    
    private func toStringMaybeNil(_ expected: JSONError.ExpectedType) throws -> String? {
        switch self {
        case .string(let s): return s
        case .null: return nil
        case .bool(let b): return String(b)
        case .int64(let i): return String(i)
        case .double(let d): return String(d)
        default: throw JSONError.missingOrInvalidType(path: nil, expected: expected, actual: .forValue(self))
        }
    }
    
    /// Returns the receiver coerced to a 64-bit integral value.
    /// If the receiver is a floating-point value, the value will be truncated
    /// to an integer.
    /// - Returns: An `Int64` value`.
    /// - Throws: `JSONError` if the receiver is `null`, a boolean, an object,
    ///   an array, a string that cannot be coerced to a 64-bit integral value,
    ///   or a floating-point value that does not fit in 64 bits.
    func toInt64() throws -> Int64 {
        guard let val = try toInt64MaybeNil(.required(.number)) else {
            throw JSONError.missingOrInvalidType(path: nil, expected: .required(.number), actual: .null)
        }
        return val
    }
    
    /// Returns the receiver coerced to a 64-bit integral value.
    /// If the receiver is a floating-point value, the value will be truncated
    /// to an integer.
    /// - Returns: An `Int64` value`, or `nil` if the receiver is `null`.
    /// - Throws: `JSONError` if the receiver is a boolean, an object, an array,
    ///   a string that cannot be coerced to a 64-bit integral value,
    ///   or a floating-point value that does not fit in 64 bits.
    func toInt64OrNil() throws -> Int64? {
        return try toInt64MaybeNil(.optional(.number))
    }
    
    private func toInt64MaybeNil(_ expected: JSONError.ExpectedType) throws -> Int64? {
        switch self {
        case .int64(let i):
            return i
        case .double(let d):
            guard let val = convertDoubleToInt64(d) else {
                throw JSONError.outOfRangeDouble(path: nil, value: d, expected: Int64.self)
            }
            return val
        case .string(let s):
            if let i = Int64(s, radix: 10) {
                return i
            } else if let d = Double(s) {
                guard let val = convertDoubleToInt64(d) else {
                    throw JSONError.outOfRangeDouble(path: nil, value: d, expected: Int64.self)
                }
                return val
            }
        case .null:
            return nil
        default:
            break
        }
        throw JSONError.missingOrInvalidType(path: nil, expected: expected, actual: .forValue(self))
    }
    
    /// Returns the receiver coerced to an integral value.
    /// If the receiver is a floating-point value, the value will be truncated
    /// to an integer.
    /// - Returns: An `Int` value`.
    /// - Throws: `JSONError` if the receiver is `null`, a boolean, an object,
    ///   an array, a string that cannot be coerced to an integral value,
    ///   or a floating-point value that does not fit in an `Int`.
    func toInt() throws -> Int {
        let val = try toInt64()
        let truncated = Int(truncatingBitPattern: val)
        guard Int64(truncated) == val else { throw JSONError.outOfRangeInt64(path: nil, value: val, expected: Int.self) }
        return truncated
    }
    
    /// Returns the receiver coerced to an integral value.
    /// If the receiver is a floating-point value, the value will be truncated
    /// to an integer.
    /// - Returns: An `Int` value`, or `nil` if the receiver is `null`.
    /// - Throws: `JSONError` if the receiver is a boolean, an object,
    ///   an array, a string that cannot be coerced to an integral value,
    ///   or a floating-point value that does not fit in an `Int`.
    func toIntOrNil() throws -> Int? {
        guard let val = try toInt64OrNil() else { return nil }
        let truncated = Int(truncatingBitPattern: val)
        guard Int64(truncated) == val else { throw JSONError.outOfRangeInt64(path: nil, value: val, expected: Int.self) }
        return truncated
    }
    
    /// Returns the receiver coerced to a `Double`.
    /// - Returns: A `Double` value.
    /// - Throws: `JSONError` if the receiver is `null`, a boolean, an object, an array,
    ///   or a string that cannot be coerced to a floating-point value.
    func toDouble() throws -> Double {
        guard let val = try toDoubleMaybeNil(.required(.number)) else {
            throw JSONError.missingOrInvalidType(path: nil, expected: .required(.number), actual: .null)
        }
        return val
    }
    
    /// Returns the receiver coerced to a `Double`.
    /// - Returns: A `Double` value, or `nil` if the receiver is `null`.
    /// - Throws: `JSONError` if the receiver is a boolean, an object, an array,
    ///   or a string that cannot be coerced to a floating-point value.
    func toDoubleOrNil() throws -> Double? {
        return try toDoubleMaybeNil(.optional(.number))
    }
    
    private func toDoubleMaybeNil(_ expected: JSONError.ExpectedType) throws -> Double? {
        switch self {
        case .int64(let i): return Double(i)
        case .double(let d): return d
        case .string(let s): return Double(s)
        case .null: return nil
        default: throw JSONError.missingOrInvalidType(path: nil, expected: expected, actual: .forValue(self))
        }
    }
}

// MARK: - Keyed accessors
public extension JSON {
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `Bool` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type, or if
    ///   the receiver is not an object.
    func getBool(_ key: String) throws -> Bool {
        let dict = try getObject()
        let value = try getRequired(dict, key: key, type: .string)
        return try scoped(key) { try value.getBool() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `Bool` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an object.
    func getBoolOrNil(_ key: String) throws -> Bool? {
        let dict = try getObject()
        guard let value = dict[key] else { return nil }
        return try scoped(key) { try value.getBoolOrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `String` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type, or if
    ///   the receiver is not an object.
    func getString(_ key: String) throws -> String {
        let dict = try getObject()
        let value = try getRequired(dict, key: key, type: .string)
        return try scoped(key) { try value.getString() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `String` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an object.
    func getStringOrNil(_ key: String) throws -> String? {
        let dict = try getObject()
        guard let value = dict[key] else { return nil }
        return try scoped(key) { try value.getStringOrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int64` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type, or if
    ///   the receiver is not an object.
    func getInt64(_ key: String) throws -> Int64 {
        let dict = try getObject()
        let value = try getRequired(dict, key: key, type: .number)
        return try scoped(key) { try value.getInt64() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int64` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an object.
    func getInt64OrNil(_ key: String) throws -> Int64? {
        let dict = try getObject()
        guard let value = dict[key] else { return nil }
        return try scoped(key) { try value.getInt64OrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type,
    ///   or if the 64-bit integral value is too large to fit in an `Int`, or if
    ///   the receiver is not an object.
    func getInt(_ key: String) throws -> Int {
        let dict = try getObject()
        let value = try getRequired(dict, key: key, type: .number)
        return try scoped(key) { try value.getInt() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the 64-bit integral
    ///   value is too large to fit in an `Int`, or if the receiver is not an object.
    func getIntOrNil(_ key: String) throws -> Int? {
        let dict = try getObject()
        guard let value = dict[key] else { return nil }
        return try scoped(key) { try value.getIntOrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `Double` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type, or if
    ///   the receiver is not an object.
    func getDouble(_ key: String) throws -> Double {
        let dict = try getObject()
        let value = try getRequired(dict, key: key, type: .number)
        return try scoped(key) { try value.getDouble() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `Double` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an object.
    func getDoubleOrNil(_ key: String) throws -> Double? {
        let dict = try getObject()
        guard let value = dict[key] else { return nil }
        return try scoped(key) { try value.getDoubleOrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Note: Use `getObject(_:_:)` when using throwing accessors on the resulting
    ///   object value to produce better errors.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An object value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type, or if
    ///   the receiver is not an object.
    /// - SeeAlso: `getObject(_:_:)`
    func getObject(_ key: String) throws -> JSONObject {
        return try getObject(key, { $0 })
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Note: Use `getObjectOrNil(_:_:)` when using throwing accessors on the resulting
    ///   object value to produce better errors.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An object value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an object.
    /// - SeeAlso: `getObjectOrNil(_:_:)`
    func getObjectOrNil(_ key: String) throws -> JSONObject? {
        return try getObjectOrNil(key, { $0 })
    }
    
    /// Subscripts the receiver with `key` and passes the result to the given block.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Parameter transform: A block that's called with the result of subscripting the receiver with `key`.
    /// - Returns: The result of calling the given block.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type, or if
    ///   the receiver is not an object, or any error thrown by `transform`.
    func getObject<T>(_ key: String, _ transform: (JSONObject) throws -> T) throws -> T {
        return try getObject().getObject(key, transform)
    }
    
    /// Subscripts the receiver with `key` and passes the result to the given block.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Parameter transform: A block that's called with the result of subscripting the receiver with `key`.
    /// - Returns: The result of calling the given block, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an object,
    ///   or any error thrown by `transform`.
    func getObjectOrNil<T>(_ key: String, _ transform: (JSONObject) throws -> T?) throws -> T? {
        return try getObject().getObjectOrNil(key, transform)
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Note: Use `getArray(_:_:)` when using throwing accessors on the resulting
    ///   array value to produce better errors.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type, or if
    ///   the receiver is not an object.
    /// - SeeAlso: `getArray(_:_:)`
    func getArray(_ key: String) throws -> JSONArray {
        return try getArray(key, { $0 })
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Note: Use `getArrayOrNil(_:_:)` when using throwing accessors on the resulting
    ///   array value to produce better errors.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An array value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type.
    /// - SeeAlso: `getArrayOrNil(_:_:)`
    func getArrayOrNil(_ key: String) throws -> JSONArray? {
        return try getArrayOrNil(key, { $0 })
    }
    
    /// Subscripts the receiver with `key` and passes the result to the given block.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Parameter transform: A block that's called with the result of subscripting the receiver with `key`.
    /// - Returns: The result of calling the given block.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type, or if
    ///   the receiver is not an object, or any error thrown by `transform`.
    func getArray<T>(_ key: String, _ transform: (JSONArray) throws -> T) throws -> T {
        let dict = try getObject()
        let value = try getRequired(dict, key: key, type: .array)
        return try scoped(key) { try transform(value.getArray()) }
    }
    
    /// Subscripts the receiver with `key` and passes the result to the given block.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Parameter transform: A block that's called with the result of subscripting the receiver with `key`.
    /// - Returns: The result of calling the given block, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an object,
    ///   or any error thrown by `transform`.
    func getArrayOrNil<T>(_ key: String, _ transform: (JSONArray) throws -> T?) throws -> T? {
        let dict = try getObject()
        guard let value = dict[key] else { return nil }
        return try scoped(key) { try value.getArrayOrNil().flatMap(transform) }
    }
}

public extension JSON {
    /// Subscripts the receiver with `key` and returns the result coerced to a `String`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `String` value.
    /// - Throws: `JSONError` if the key doesn't exist, the value is an object or an array,
    ///   or if the receiver is not an object.
    /// - SeeAlso: `toString()`.
    func toString(_ key: String) throws -> String {
        let dict = try getObject()
        let value = try getRequired(dict, key: key, type: .string)
        return try scoped(key) { try value.toString() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to a `String`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `String` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value is an object or an array, or if the receiver is not an object.
    /// - SeeAlso: `toStringOrNil()`.
    func toStringOrNil(_ key: String) throws -> String? {
        let dict = try getObject()
        guard let value = dict[key] else { return nil }
        return try scoped(key) { try value.toStringOrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to an `Int64`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int64` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is `null`, a boolean, an object,
    ///   an array, a string that cannot be coerced to a 64-bit integral value, or a floating-point
    ///   value that does not fit in 64 bits, or if the receiver is not an object.
    func toInt64(_ key: String) throws -> Int64 {
        let dict = try getObject()
        let value = try getRequired(dict, key: key, type: .number)
        return try scoped(key) { try value.toInt64() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to an `Int64`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int64` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the the value is a boolean, an object, an array, a string
    ///   that cannot be coerced to a 64-bit integral value, or a floating-point value
    ///   that does not fit in 64 bits, or if the receiver is not an object.
    func toInt64OrNil(_ key: String) throws -> Int64? {
        let dict = try getObject()
        guard let value = dict[key] else { return nil }
        return try scoped(key) { try value.toInt64OrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to an `Int`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is `null`, a boolean, an object,
    ///   an array, a string that cannot be coerced to an integral value, or a floating-point
    ///   value that does not fit in an `Int`, or if the receiver is not an object.
    func toInt(_ key: String) throws -> Int {
        let dict = try getObject()
        let value = try getRequired(dict, key: key, type: .number)
        return try scoped(key) { try value.toInt() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to an `Int`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the the value is a boolean, an object, an array, a string
    ///   that cannot be coerced to an integral value, or a floating-point value
    ///   that does not fit in an `Int`, or if the receiver is not an object.
    func toIntOrNil(_ key: String) throws -> Int? {
        let dict = try getObject()
        guard let value = dict[key] else { return nil }
        return try scoped(key) { try value.toIntOrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to a `Double`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `Double` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is `null`, a boolean,
    ///   an object, an array, or a string that cannot be coerced to a floating-point value,
    ///   or if the receiver is not an object.
    func toDouble(_ key: String) throws -> Double {
        let dict = try getObject()
        let value = try getRequired(dict, key: key, type: .number)
        return try scoped(key) { try value.toDouble() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to a `Double`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `Double` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value is a boolean, an object, an array, or a string that
    ///   cannot be coerced to a floating-point value, or if the receiver is not an object.
    func toDoubleOrNil(_ key: String) throws -> Double? {
        let dict = try getObject()
        guard let value = dict[key] else { return nil }
        return try scoped(key) { try value.toDoubleOrNil() }
    }
}

// MARK: - Indexed accessors
public extension JSON {
    /// Subscripts the receiver with `index` and returns the result.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: A `Bool` value.
    /// - Throws: `JSONError` if the index is out of bounds or the value is the wrong type,
    ///   or if the receiver is not an array.
    func getBool(_ index: Int) throws -> Bool {
        let ary = try getArray()
        let value = try getRequired(ary, index: index, type: .bool)
        return try scoped(index) { try value.getBool() }
    }
    
    /// Subscripts the receiver with `index` and returns the result.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: A `Bool` value, or `nil` if the index is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an array.
    func getBoolOrNil(_ index: Int) throws -> Bool? {
        let ary = try getArray()
        guard let value = ary[safe: index] else { return nil }
        return try scoped(index) { try value.getBoolOrNil() }
    }
    
    /// Subscripts the receiver with `index` and returns the result.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: A `String` value.
    /// - Throws: `JSONError` if the index is out of bounds or the value is the wrong type,
    ///   or if the receiver is not an array.
    func getString(_ index: Int) throws -> String {
        let ary = try getArray()
        let value = try getRequired(ary, index: index, type: .string)
        return try scoped(index) { try value.getString() }
    }
    
    /// Subscripts the receiver with `index` and returns the result.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: A `String` value, or `nil` if the index is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an array.
    func getStringOrNil(_ index: Int) throws -> String? {
        let ary = try getArray()
        guard let value = ary[safe: index] else { return nil }
        return try scoped(index) { try value.getStringOrNil() }
    }
    
    /// Subscripts the receiver with `index` and returns the result.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: An `Int64` value.
    /// - Throws: `JSONError` if the index is out of bounds or the value is the wrong type,
    ///   or if the receiver is not an array.
    func getInt64(_ index: Int) throws -> Int64 {
        let ary = try getArray()
        let value = try getRequired(ary, index: index, type: .number)
        return try scoped(index) { try value.getInt64() }
    }
    
    /// Subscripts the receiver with `index` and returns the result.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: An `Int64` value, or `nil` if the index is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an array.
    func getInt64OrNil(_ index: Int) throws -> Int64? {
        let ary = try getArray()
        guard let value = ary[safe: index] else { return nil }
        return try scoped(index) { try value.getInt64OrNil() }
    }
    
    /// Subscripts the receiver with `index` and returns the result.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: An `Int` value.
    /// - Throws: `JSONError` if the index is out of bounds or the value is the wrong type,
    ///   or if the 64-bit integral value is too large to fit in an `Int`, or if
    ///   the receiver is not an array.
    func getInt(_ index: Int) throws -> Int {
        let ary = try getArray()
        let value = try getRequired(ary, index: index, type: .number)
        return try scoped(index) { try value.getInt() }
    }
    
    /// Subscripts the receiver with `index` and returns the result.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: An `Int` value, or `nil` if the index is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the 64-bit integral value
    ///   is too large to fit in an `Int`, or if the receiver is not an array.
    func getIntOrNil(_ index: Int) throws -> Int? {
        let ary = try getArray()
        guard let value = ary[safe: index] else { return nil }
        return try scoped(index) { try value.getIntOrNil() }
    }
    
    /// Subscripts the receiver with `index` and returns the result.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: A `Double` value.
    /// - Throws: `JSONError` if the index is out of bounds or the value is the wrong type,
    ///   or if the receiver is not an array.
    func getDouble(_ index: Int) throws -> Double {
        let ary = try getArray()
        let value = try getRequired(ary, index: index, type: .number)
        return try scoped(index) { try value.getDouble() }
    }
    
    /// Subscripts the receiver with `index` and returns the result.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: A `Double` value, or `nil` if the index is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an array.
    func getDoubleOrNil(_ index: Int) throws -> Double? {
        let ary = try getArray()
        guard let value = ary[safe: index] else { return nil }
        return try scoped(index) { try value.getDoubleOrNil() }
    }
    
    /// Subscripts the receiver with `index` and returns the result.
    /// - Note: Use `getObject(_:_:)` when using throwing accessors on the resulting
    ///   object value to produce better errors.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: An object value.
    /// - Throws: `JSONError` if the index is out of bounds or the value is the wrong type,
    ///   or if the receiver is not an array.
    /// - SeeAlso: `getObject(_:_:)`
    func getObject(_ index: Int) throws -> JSONObject {
        return try getObject(index, { $0 })
    }
    
    /// Subscripts the receiver with `index` and returns the result.
    /// - Note: Use `getObjectOrNil(_:_:)` when using throwing accessors on the resulting
    ///   object value to produce better errors.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: An object value, or `nil` if the index is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an array.
    /// - SeeAlso: `getObjectOrNil(_:_:)`
    func getObjectOrNil(_ index: Int) throws -> JSONObject? {
        return try getObjectOrNil(index, { $0 })
    }
    
    /// Subscripts the receiver with `index` and passes the result to the given block.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Parameter transform: A block that's called with the result of subscripting the receiver with `index`.
    /// - Returns: The result of calling the given block.
    /// - Throws: `JSONError` if the index is out of bounds or the value is the wrong type,
    ///   or if the receiver is not an array, or any error thrown by `transform`.
    func getObject<T>(_ index: Int, _ f: (JSONObject) throws -> T) throws -> T {
        let ary = try getArray()
        let value = try getRequired(ary, index: index, type: .object)
        return try scoped(index) { try f(value.getObject()) }
    }
    
    /// Subscripts the receiver with `index` and passes the result to the given block.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Parameter transform: A block that's called with the result of subscripting the receiver with `index`.
    /// - Returns: The result of calling the given block, or `nil` if the index is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an array,
    ////  or any error thrown by `transform`.
    func getObjectOrNil<T>(_ index: Int, _ f: (JSONObject) throws -> T?) throws -> T? {
        let ary = try getArray()
        guard let value = ary[safe: index] else { return nil }
        return try scoped(index) { try value.getObjectOrNil().flatMap(f) }
    }
    
    /// Subscripts the receiver with `index` and returns the result.
    /// - Note: Use `getArray(_:_:)` when using throwing accessors on the resulting
    ///   array value to produce better errors.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: An array value.
    /// - Throws: `JSONError` if the index is out of bounds or the value is the wrong type,
    ///   or if the receiver is not an array.
    /// - SeeAlso: `getArray(_:_:)`
    func getArray(_ index: Int) throws -> JSONArray {
        return try getArray(index, { $0 })
    }
    
    /// Subscripts the receiver with `index` and returns the result.
    /// - Note: Use `getArrayOrNil(_:_:)` when using throwing accessors on the resulting
    ///   array value to produce better errors.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: An array value, or `nil` if the index is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an array.
    /// - SeeAlso: `getArrayOrNil(_:_:)`
    func getArrayOrNil(_ index: Int) throws -> JSONArray? {
        return try getArrayOrNil(index, { $0 })
    }
    
    /// Subscripts the receiver with `index` and passes the result to the given block.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Parameter transform: A block that's called with the result of subscripting the receiver with `index`.
    /// - Returns: The result of calling the given block.
    /// - Throws: `JSONError` if the index is out of bounds or the value is the wrong type,
    ///   or if the receiver is not an array, or any error thrown by `transform`.
    func getArray<T>(_ index: Int, _ f: (JSONArray) throws -> T) throws -> T {
        let ary = try getArray()
        let value = try getRequired(ary, index: index, type: .array)
        return try scoped(index) { try f(value.getArray()) }
    }
    
    /// Subscripts the receiver with `index` and passes the result to the given block.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Parameter transform: A block that's called with the result of subscripting the receiver with `index`.
    /// - Returns: The result of calling the given block, or `nil` if the index is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the receiver is not an array,
    ///   or any error thrown by `transform`.
    func getArrayOrNil<T>(_ index: Int, _ f: (JSONArray) throws -> T?) throws -> T? {
        let ary = try getArray()
        guard let value = ary[safe: index] else { return nil }
        return try scoped(index) { try value.getArrayOrNil().flatMap(f) }
    }
}

public extension JSON {
    /// Subscripts the receiver with `index` and returns the result coerced to a `String`.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: A `String` value.
    /// - Throws: `JSONError` if the index is out of bounds or the value is an object or an array,
    ///   or if the receiver is not an array.
    /// - SeeAlso: `toString()`.
    func toString(_ index: Int) throws -> String {
        let ary = try getArray()
        let value = try getRequired(ary, index: index, type: .string)
        return try scoped(index) { try value.toString() }
    }
    
    /// Subscripts the receiver with `index` and returns the result coerced to a `String`.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: A `String` value, or `nil` if the index is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the value is an object or an array, or if the receiver is not an array.
    /// - SeeAlso: `toStringOrNil()`.
    func toStringOrNil(_ index: Int) throws -> String? {
        let ary = try getArray()
        guard let value = ary[safe: index] else { return nil }
        return try scoped(index) { try value.toStringOrNil() }
    }
    
    /// Subscripts the receiver with `index` and returns the result coerced to an `Int64`.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: An `Int64` value.
    /// - Throws: `JSONError` if the index is out of bounds or the value is `null`, a boolean,
    ///   an object, an array, a string that cannot be coerced to a 64-bit integral value, or a
    ///   floating-point value that does not fit in 64 bits, or if the receiver is not an array.
    /// - SeeAlso: `toInt64()`.
    func toInt64(_ index: Int) throws -> Int64 {
        let ary = try getArray()
        let value = try getRequired(ary, index: index, type: .number)
        return try scoped(index) { try value.toInt64() }
    }
    
    /// Subscripts the receiver with `index` and returns the result coerced to an `Int64`.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: An `Int64` value, or `nil` if the index is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the the value is a boolean, an object, an array, a string
    ///   that cannot be coerced to a 64-bit integral value, or a floating-point value
    ///   that does not fit in 64 bits, or if the receiver is not an array.
    /// - SeeAlso: `toInt64OrNil()`.
    func toInt64OrNil(_ index: Int) throws -> Int64? {
        let ary = try getArray()
        guard let value = ary[safe: index] else { return nil }
        return try scoped(index) { try value.toInt64OrNil() }
    }
    
    /// Subscripts the receiver with `index` and returns the result coerced to an `Int`.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: An `Int` value.
    /// - Throws: `JSONError` if the index is out of bounds or the value is `null`, a boolean,
    ///   an object, an array, a string that cannot be coerced to an integral value, or a
    ///   floating-point value that does not fit in an `Int`, or if the receiver is not an array.
    /// - SeeAlso: `toInt()`.
    func toInt(_ index: Int) throws -> Int {
        let ary = try getArray()
        let value = try getRequired(ary, index: index, type: .number)
        return try scoped(index) { try value.toInt() }
    }
    
    /// Subscripts the receiver with `index` and returns the result coerced to an `Int`.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: An `Int` value, or `nil` if the index is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the the value is a boolean, an object, an array, a string
    ///   that cannot be coerced to an integral value, or a floating-point value
    ///   that does not fit in an `Int`, or if the receiver is not an array.
    /// - SeeAlso: `toIntOrNil()`.
    func toIntOrNil(_ index: Int) throws -> Int? {
        let ary = try getArray()
        guard let value = ary[safe: index] else { return nil }
        return try scoped(index) { try value.toIntOrNil() }
    }
    
    /// Subscripts the receiver with `index` and returns the result coerced to a `Double`.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: A `Double` value.
    /// - Throws: `JSONError` if the index is out of bounds or the value is `null`, a boolean,
    ///   an object, an array, or a string that cannot be coerced to a floating-point value,
    ///   or if the receiver is not an array.
    /// - SeeAlso: `toDouble()`.
    func toDouble(_ index: Int) throws -> Double {
        let ary = try getArray()
        let value = try getRequired(ary, index: index, type: .number)
        return try scoped(index) { try value.toDouble() }
    }
    
    /// Subscripts the receiver with `index` and returns the result coerced to a `Double`.
    /// - Parameter index: The index that's used to subscript the receiver.
    /// - Returns: A `Double` value, or `nil` if the index is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the value is a boolean, an object, an array, or a string that
    ///   cannot be coerced to a floating-point value, or if the receiver is not an array.
    /// - SeeAlso: `toDouble()`.
    func toDoubleOrNil(_ index: Int) throws -> Double? {
        let ary = try getArray()
        guard let value = ary[safe: index] else { return nil }
        return try scoped(index) { try value.toDoubleOrNil() }
    }
}

// MARK: -

public extension JSONObject {
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `Bool` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type.
    func getBool(_ key: String) throws -> Bool {
        let value = try getRequired(self, key: key, type: .string)
        return try scoped(key) { try value.getBool() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `Bool` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type.
    func getBoolOrNil(_ key: String) throws -> Bool? {
        guard let value = self[key] else { return nil }
        return try scoped(key) { try value.getBoolOrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `String` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type.
    func getString(_ key: String) throws -> String {
        let value = try getRequired(self, key: key, type: .string)
        return try scoped(key) { try value.getString() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `String` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type.
    func getStringOrNil(_ key: String) throws -> String? {
        guard let value = self[key] else { return nil }
        return try scoped(key) { try value.getStringOrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int64` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type.
    func getInt64(_ key: String) throws -> Int64 {
        let value = try getRequired(self, key: key, type: .number)
        return try scoped(key) { try value.getInt64() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int64` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type.
    func getInt64OrNil(_ key: String) throws -> Int64? {
        guard let value = self[key] else { return nil }
        return try scoped(key) { try value.getInt64OrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type,
    ///   or if the 64-bit integral value is too large to fit in an `Int`.
    func getInt(_ key: String) throws -> Int {
        let value = try getRequired(self, key: key, type: .number)
        return try scoped(key) { try value.getInt() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or if the 64-bit integral
    ///   value is too large to fit in an `Int`.
    func getIntOrNil(_ key: String) throws -> Int? {
        guard let value = self[key] else { return nil }
        return try scoped(key) { try value.getIntOrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `Double` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type.
    func getDouble(_ key: String) throws -> Double {
        let value = try getRequired(self, key: key, type: .number)
        return try scoped(key) { try value.getDouble() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `Double` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type.
    func getDoubleOrNil(_ key: String) throws -> Double? {
        guard let value = self[key] else { return nil }
        return try scoped(key) { try value.getDoubleOrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Note: Use `getObject(_:_:)` when using throwing accessors on the resulting
    ///   object value to produce better errors.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An object value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type.
    /// - SeeAlso: `getObject(_:_:)`
    func getObject(_ key: String) throws -> JSONObject {
        return try getObject(key, { $0 })
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Note: Use `getObjectOrNil(_:_:)` when using throwing accessors on the resulting
    ///   object value to produce better errors.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An object value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type.
    /// - SeeAlso: `getObjectOrNil(_:_:)`
    func getObjectOrNil(_ key: String) throws -> JSONObject? {
        return try getObjectOrNil(key, { $0 })
    }
    
    /// Subscripts the receiver with `key` and passes the result to the given block.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Parameter transform: A block that's called with the result of subscripting the receiver with `key`.
    /// - Returns: The result of calling the given block.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type, or any
    ///   error thrown by `transform`.
    func getObject<T>(_ key: String, _ f: (JSONObject) throws -> T) throws -> T {
        let value = try getRequired(self, key: key, type: .object)
        return try scoped(key) { try f(value.getObject()) }
    }
    
    /// Subscripts the receiver with `key` and passes the result to the given block.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Parameter transform: A block that's called with the result of subscripting the receiver with `key`.
    /// - Returns: The result of calling the given block, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or any error thrown by `transform`.
    func getObjectOrNil<T>(_ key: String, _ f: (JSONObject) throws -> T?) throws -> T? {
        guard let value = self[key] else { return nil }
        return try scoped(key) { try value.getObjectOrNil().flatMap(f) }
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Note: Use `getArray(_:_:)` when using throwing accessors on the resulting
    ///   array value to produce better errors.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type.
    /// - SeeAlso: `getArray(_:_:)`
    func getArray(_ key: String) throws -> JSONArray {
        return try getArray(key, { $0 })
    }
    
    /// Subscripts the receiver with `key` and returns the result.
    /// - Note: Use `getArrayOrNil(_:_:)` when using throwing accessors on the resulting
    ///   array value to produce better errors.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An array value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type.
    /// - SeeAlso: `getArrayOrNil(_:_:)`
    func getArrayOrNil(_ key: String) throws -> JSONArray? {
        return try getArrayOrNil(key, { $0 })
    }
    
    /// Subscripts the receiver with `key` and passes the result to the given block.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Parameter transform: A block that's called with the result of subscripting the receiver with `key`.
    /// - Returns: The result of calling the given block.
    /// - Throws: `JSONError` if the key doesn't exist or the value is the wrong type, or any
    ///   error thrown by `transform`.
    func getArray<T>(_ key: String, _ f: (JSONArray) throws -> T) throws -> T {
        let value = try getRequired(self, key: key, type: .array)
        return try scoped(key) { try f(value.getArray()) }
    }
    
    /// Subscripts the receiver with `key` and passes the result to the given block.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Parameter transform: A block that's called with the result of subscripting the receiver with `key`.
    /// - Returns: The result of calling the given block, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value has the wrong type, or any error thrown by `transform`.
    func getArrayOrNil<T>(_ key: String, _ f: (JSONArray) throws -> T?) throws -> T? {
        guard let value = self[key] else { return nil }
        return try scoped(key) { try value.getArrayOrNil().flatMap(f) }
    }
}

public extension JSONObject {
    /// Subscripts the receiver with `key` and returns the result coerced to a `String`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `String` value.
    /// - Throws: `JSONError` if the key doesn't exist, the value is an object or an array,
    ///   or if the receiver is not an object.
    /// - SeeAlso: `toString()`.
    func toString(_ key: String) throws -> String {
        let value = try getRequired(self, key: key, type: .string)
        return try scoped(key) { try value.toString() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to a `String`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `String` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value is an object or an array, or if the receiver is not an object.
    /// - SeeAlso: `toStringOrNil()`.
    func toStringOrNil(_ key: String) throws -> String? {
        guard let value = self[key] else { return nil }
        return try scoped(key) { try value.toStringOrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to an `Int64`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int64` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is `null`, a boolean, an object,
    ///   an array, a string that cannot be coerced to a 64-bit integral value, or a floating-point
    ///   value that does not fit in 64 bits, or if the receiver is not an object.
    func toInt64(_ key: String) throws -> Int64 {
        let value = try getRequired(self, key: key, type: .number)
        return try scoped(key) { try value.toInt64() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to an `Int64`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int64` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the the value is a boolean, an object, an array, a string
    ///   that cannot be coerced to a 64-bit integral value, or a floating-point value
    ///   that does not fit in 64 bits, or if the receiver is not an object.
    func toInt64OrNil(_ key: String) throws -> Int64? {
        guard let value = self[key] else { return nil }
        return try scoped(key) { try value.toInt64OrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to an `Int`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is `null`, a boolean, an object,
    ///   an array, a string that cannot be coerced to an integral value, or a floating-point
    ///   value that does not fit in an `Int`, or if the receiver is not an object.
    func toInt(_ key: String) throws -> Int {
        let value = try getRequired(self, key: key, type: .number)
        return try scoped(key) { try value.toInt() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to an `Int`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: An `Int` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the the value is a boolean, an object, an array, a string
    ///   that cannot be coerced to an integral value, or a floating-point value
    ///   that does not fit in an `Int`, or if the receiver is not an object.
    func toIntOrNil(_ key: String) throws -> Int? {
        guard let value = self[key] else { return nil }
        return try scoped(key) { try value.toIntOrNil() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to a `Double`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `Double` value.
    /// - Throws: `JSONError` if the key doesn't exist or the value is `null`, a boolean,
    ///   an object, an array, or a string that cannot be coerced to a floating-point value,
    ///   or if the receiver is not an object.
    func toDouble(_ key: String) throws -> Double {
        let value = try getRequired(self, key: key, type: .number)
        return try scoped(key) { try value.toDouble() }
    }
    
    /// Subscripts the receiver with `key` and returns the result coerced to a `Double`.
    /// - Parameter key: The key that's used to subscript the receiver.
    /// - Returns: A `Double` value, or `nil` if the key doesn't exist or the value is `null`.
    /// - Throws: `JSONError` if the value is a boolean, an object, an array, or a string that
    ///   cannot be coerced to a floating-point value, or if the receiver is not an object.
    func toDoubleOrNil(_ key: String) throws -> Double? {
        guard let value = self[key] else { return nil }
        return try scoped(key) { try value.toDoubleOrNil() }
    }
}

// MARK: - JSONArray helpers

public extension JSON {
    /// Returns an `Array` containing the results of mapping `transform` over `array`.
    ///
    /// If `transform` throws a `JSONError`, the error will be modified to include the index
    /// of the element that caused the error.
    ///
    /// - Parameter array: The `JSONArray` to map over.
    /// - Parameter transform: A block that is called once for each element of `array`.
    /// - Returns: An array with the results of mapping `transform` over `array`.
    /// - Throws: Rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    static func map<T>(_ array: JSONArray, _ transform: (JSON) throws -> T) rethrows -> [T] {
        return try array.enumerated().map({ i, elt in try scoped(i, { try transform(elt) }) })
    }
    
    /// Returns an `Array` containing the non-`nil` results of mapping `transform` over `array`.
    ///
    /// If `transform` throws a `JSONError`, the error will be modified to include the index
    /// of the element that caused the error.
    ///
    /// - Parameter array: The `JSONArray` to map over.
    /// - Parameter transform: A block that is called once for each element of `array`.
    /// - Returns: An array with the non-`nil` results of mapping `transform` over `array`.
    /// - Throws: Rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    static func flatMap<T>(_ array: JSONArray, _ transform: (JSON) throws -> T?) rethrows -> [T] {
        return try array.enumerated().flatMap({ i, elt in try scoped(i, { try transform(elt) }) })
    }
    
    /// Returns an `Array` containing the concatenated results of mapping `transform` over `array`.
    ///
    /// If `transform` throws a `JSONError`, the error will be modified to include the index
    /// of the element that caused the error.
    ///
    /// - Parameter array: The `JSONArray` to map over.
    /// - Parameter transform: A block that is called once for each element of `array`.
    /// - Returns: An array with the concatenated results of mapping `transform` over `array`.
    /// - Throws: Rethrows any error thrown by `transform`.
    /// - Complexity: O(*M* + *N*) where *M* is the length of `array` and *N* is the length of the result.
    static func flatMap<S: Sequence>(_ array: JSONArray, _ transform: (JSON) throws -> S) rethrows -> [S.Iterator.Element] {
        return try array.enumerated().flatMap({ (i, elt) in
            return try scoped(i, { try transform(elt) })
        })
    }
}

public extension JSON {
    /// Subscripts the receiver with `key`, converts the value to an array, and returns an `Array`
    /// containing the results of mapping `transform` over the value.
    ///
    /// - Note: This method is equivalent to `getArray(key, { try JSON.map($0, transform) })`.
    ///
    /// - Parameter key: The key to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the results of mapping `transform` over the array.
    /// - Throws: `JSONError` if the receiver is not an object, `key` does not exist, or the value
    ///   is not an array. Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    func mapArray<T>(_ key: String, _ transform: (JSON) throws -> T) throws -> [T] {
        return try getArray(key, { try JSON.map($0, transform) })
    }
    
    /// Subscripts the receiver with `index`, converts the value to an array, and returns an `Array`
    /// containing the results of mapping `transform` over the value.
    ///
    /// - Note: This method is equivalent to `getArray(index, { try JSON.map($0, transform) })`.
    ///
    /// - Parameter index: The index to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the results of mapping `transform` over the array.
    /// - Throws: `JSONError` if the receiver is not an array, `index` is out of bounds, or the
    ///   value is not an array. Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    func mapArray<T>(_ index: Int, _ transform: (JSON) throws -> T) throws -> [T] {
        return try getArray(index, { try JSON.map($0, transform) })
    }
    
    /// Subscripts the receiver with `key`, converts the value to an array, and returns an `Array`
    /// containing the results of mapping `transform` over the value.
    ///
    /// Returns `nil` if `key` doesn't exist or the value is `null`.
    ///
    /// - Note: This method is equivalent to `getArrayOrNil(key, { try JSON.map($0, transform) })`.
    ///
    /// - Parameter key: The key to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the results of mapping `transform` over the array, or `nil` if
    ///   `key` does not exist or the value is `null`.
    /// - Throws: `JSONError` if the receiver is not an object or `key` exists but the value is not
    ///   an array or `null`. Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    func mapArrayOrNil<T>(_ key: String, _ transform: (JSON) throws -> T) throws -> [T]? {
        return try getArrayOrNil(key, { try JSON.map($0, transform) })
    }
    
    /// Subscripts the receiver with `index`, converts the value to an array, and returns an `Array`
    /// containing the results of mapping `transform` over the value.
    ///
    /// Returns `nil` if `index` is out of bounds or the value is `null`.
    ///
    /// - Note: This method is equivalent to `getArrayOrNil(index, { try JSON.map($0, transform) })`.
    ///
    /// - Parameter index: The index to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the results of mapping `transform` over the array, or `nil` if
    ///   `index` is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the receiver is not an object or the subscript value is not an
    ///   array or `null`. Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    func mapArrayOrNil<T>(_ index: Int, _ transform: (JSON) throws -> T) throws -> [T]? {
        return try getArrayOrNil(index, { try JSON.map($0, transform) })
    }
    
    /// Subscripts the receiver with `key`, converts the value to an array, and returns an `Array`
    /// containing the non-`nil` results of mapping `transform` over the value.
    ///
    /// - Note: This method is equivalent to `getArray(key, { try JSON.flatMap($0, transform) })`.
    ///
    /// - Parameter key: The key to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the non-`nil` results of mapping `transform` over the array.
    /// - Throws: `JSONError` if the receiver is not an object, `key` does not exist, or the value
    ///   is not an array. Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    func flatMapArray<T>(_ key: String, _ transform: (JSON) throws -> T?) throws -> [T] {
        return try getArray(key, { try JSON.flatMap($0, transform) })
    }
    
    /// Subscripts the receiver with `key`, converts the value to an array, and returns an `Array`
    /// containing the concatenated results of mapping `transform` over the value.
    ///
    /// - Note: This method is equivalent to `getArray(key, { try JSON.flatMap($0, transform) })`.
    ///
    /// - Parameter key: The key to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the concatenated results of mapping `transform` over the array.
    /// - Throws: `JSONError` if the receiver is not an object, `key` does not exist, or the value
    ///   is not an array. Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*M* + *N*) where *M* is the length of `array` and *N* is the length of the result.
    func flatMapArray<S: Sequence>(_ key: String, _ transform: (JSON) throws -> S) throws -> [S.Iterator.Element] {
        return try getArray(key, { try JSON.flatMap($0, transform) })
    }
    
    /// Subscripts the receiver with `index`, converts the value to an array, and returns an `Array`
    /// containing the non-`nil` results of mapping `transform` over the value.
    ///
    /// - Note: This method is equivalent to `getArray(index, { try JSON.flatMap($0, transform) })`.
    ///
    /// - Parameter index: The index to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the non-`nil` results of mapping `transform` over the array.
    /// - Throws: `JSONError` if the receiver is not an array, `index` is out of bounds, or the
    ///   value is not an array. Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    func flatMapArray<T>(_ index: Int, _ transform: (JSON) throws -> T?) throws -> [T] {
        return try getArray(index, { try JSON.flatMap($0, transform) })
    }
    
    /// Subscripts the receiver with `index`, converts the value to an array, and returns an `Array`
    /// containing the concatenated results of mapping `transform` over the value.
    ///
    /// - Note: This method is equivalent to `getArray(index, { try JSON.flatMap($0, transform) })`.
    ///
    /// - Parameter index: The index to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the concatenated results of mapping `transform` over the array.
    /// - Throws: `JSONError` if the receiver is not an array, `index` is out of bounds, or the
    ///   value is not an array. Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*M* + *N*) where *M* is the length of `array` and *N* is the length of the result.
    func flatMapArray<S: Sequence>(_ index: Int, _ transform: (JSON) throws -> S) throws -> [S.Iterator.Element] {
        return try getArray(index, { try JSON.flatMap($0, transform) })
    }
    
    /// Subscripts the receiver with `key`, converts the value to an array, and returns an `Array`
    /// containing the non-`nil` results of mapping `transform` over the value.
    ///
    /// Returns `nil` if `key` doesn't exist or the value is `null`.
    ///
    /// - Note: This method is equivalent to `getArrayOrNil(key, { try JSON.flatMap($0, transform) })`.
    ///
    /// - Parameter key: The key to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the non-`nil` results of mapping `transform` over the array, or
    ///   `nil` if `key` does not exist or the value is `null`.
    /// - Throws: `JSONError` if the receiver is not an object or the value is not an array or
    ///   `null`. Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    func flatMapArrayOrNil<T>(_ key: String, _ transform: (JSON) throws -> T?) throws -> [T]? {
        return try getArrayOrNil(key, { try JSON.flatMap($0, transform) })
    }
    
    /// Subscripts the receiver with `key`, converts the value to an array, and returns an `Array`
    /// containing the concatenated results of mapping `transform` over the value.
    ///
    /// Returns `nil` if `key` doesn't exist or the value is `null`.
    ///
    /// - Note: This method is equivalent to `getArrayOrNil(key, { try JSON.flatMap($0, transform) })`.
    ///
    /// - Parameter key: The key to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the concatenated results of mapping `transform` over the array,
    ///   or `nil` if `key` does not exist or the value is `null`.
    /// - Throws: `JSONError` if the receiver is not an object or the value is not an array or
    ///   `null`. Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*M* + *N*) where *M* is the length of `array` and *N* is the length of the result.
    func flatMapArrayOrNil<S: Sequence>(_ key: String, _ transform: (JSON) throws -> S) throws -> [S.Iterator.Element]? {
        return try getArrayOrNil(key, { try JSON.flatMap($0, transform) })
    }
    
    /// Subscripts the receiver with `index`, converts the value to an array, and returns an `Array`
    /// containing the non-`nil` results of mapping `transform` over the value.
    ///
    /// Returns `nil` if `index` is out of bounds or the value is `null`.
    ///
    /// - Note: This method is equivalent to `getArrayOrNil(index, { try JSON.flatMap($0, transform) })`.
    ///
    /// - Parameter index: The index to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the non-`nil` results of mapping `transform` over the array, or
    ///   `nil` if `index` is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the receiver is not an array or the value is not an array or
    ///   `null`. Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    func flatMapArrayOrNil<T>(_ index: Int, _ transform: (JSON) throws -> T?) throws -> [T]? {
        return try getArrayOrNil(index, { try JSON.flatMap($0, transform) })
    }
    
    /// Subscripts the receiver with `index`, converts the value to an array, and returns an `Array`
    /// containing the concatenated results of mapping `transform` over the value.
    ///
    /// Returns `nil` if `index` is out of bounds or the value is `null`.
    ///
    /// - Note: This method is equivalent to `getArrayOrNil(index, { try JSON.flatMap($0, transform) })`.
    ///
    /// - Parameter index: The index to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the concatenated results of mapping `transform` over the array,
    ///   or `nil` if `index` is out of bounds or the value is `null`.
    /// - Throws: `JSONError` if the receiver is not an array or the value is not an array or
    ///   `null`. Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*M* + *N*) where *M* is the length of `array` and *N* is the length of the result.
    func flatMapArrayOrNil<S: Sequence>(_ index: Int, _ transform: (JSON) throws -> S) throws -> [S.Iterator.Element]? {
        return try getArrayOrNil(index, { try JSON.flatMap($0, transform) })
    }
}

public extension JSONObject {
    /// Subscripts the receiver with `key`, converts the value to an array, and returns an `Array`
    /// containing the results of mapping `transform` over the value.
    ///
    /// - Note: This method is equivalent to `getArray(key, { try JSON.map($0, transform) })`.
    ///
    /// - Parameter key: The key to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the results of mapping `transform` over the array.
    /// - Throws: `JSONError` if `key` does not exist or the value is not an array.
    ///   Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    func mapArray<T>(_ key: String, _ transform: (JSON) throws -> T) throws -> [T] {
        return try getArray(key, { try JSON.map($0, transform) })
    }
    
    /// Subscripts the receiver with `key`, converts the value to an array, and returns an `Array`
    /// containing the results of mapping `transform` over the value.
    ///
    /// Returns `nil` if `key` doesn't exist or the value is `null`.
    ///
    /// - Note: This method is equivalent to `getArrayOrNil(key, { try JSON.map($0, transform) })`.
    ///
    /// - Parameter key: The key to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the results of mapping `transform` over the array, or `nil` if
    ///   `key` does not exist or the value is `null`.
    /// - Throws: `JSONError` if `key` exists but the value is not an array or `null`.
    ///   Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    func mapArrayOrNil<T>(_ key: String, _ transform: (JSON) throws -> T) throws -> [T]? {
        return try getArrayOrNil(key, { try JSON.map($0, transform) })
    }
    
    /// Subscripts the receiver with `key`, converts the value to an array, and returns an `Array`
    /// containing the non-`nil` results of mapping `transform` over the value.
    ///
    /// - Note: This method is equivalent to `getArray(key, { try JSON.flatMap($0, transform) })`.
    ///
    /// - Parameter key: The key to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the non-`nil` results of mapping `transform` over the array.
    /// - Throws: `JSONError` if `key` does not exist or the value is not an array.
    ///   Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    func flatMapArray<T>(_ key: String, _ transform: (JSON) throws -> T?) throws -> [T] {
        return try getArray(key, { try JSON.flatMap($0, transform) })
    }
    
    /// Subscripts the receiver with `key`, converts the value to an array, and returns an `Array`
    /// containing the concatenated results of mapping `transform` over the value.
    ///
    /// - Note: This method is equivalent to `getArray(key, { try JSON.flatMap($0, transform) })`.
    ///
    /// - Parameter key: The key to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the concatenated results of mapping `transform` over the array.
    /// - Throws: `JSONError` if `key` does not exist or the value is not an array.
    ///   Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*M* + *N*) where *M* is the length of `array` and *N* is the length of the result.
    func flatMapArray<S: Sequence>(_ key: String, _ transform: (JSON) throws -> S) throws -> [S.Iterator.Element] {
        return try getArray(key, { try JSON.flatMap($0, transform) })
    }
    
    /// Subscripts the receiver with `key`, converts the value to an array, and returns an `Array`
    /// containing the non-`nil` results of mapping `transform` over the value.
    ///
    /// Returns `nil` if `key` doesn't exist or the value is `null`.
    ///
    /// - Note: This method is equivalent to `getArrayOrNil(key, { try JSON.flatMap($0, transform) })`.
    ///
    /// - Parameter key: The key to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the non-`nil` results of mapping `transform` over the array, or
    ///   `nil` if `key` does not exist or the value is `null`.
    /// - Throws: `JSONError` if `key` exists but the value is not an array or `null`.
    ///   Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*N*).
    func flatMapArrayOrNil<T>(_ key: String, _ transform: (JSON) throws -> T?) throws -> [T]? {
        return try getArrayOrNil(key, { try JSON.flatMap($0, transform) })
    }
    
    /// Subscripts the receiver with `key`, converts the value to an array, and returns an `Array`
    /// containing the concatenated results of mapping `transform` over the value.
    ///
    /// Returns `nil` if `key` doesn't exist or the value is `null`.
    ///
    /// - Note: This method is equivalent to `getArrayOrNil(key, { try JSON.flatMap($0, transform) })`.
    ///
    /// - Parameter key: The key to subscript the receiver with.
    /// - Parameter transform: A block that is called once for each element of the resulting array.
    /// - Returns: An array with the concatenated results of mapping `transform` over the array,
    ///   or `nil` if `key` does not exist or the value is `null`.
    /// - Throws: `JSONError` if `key` exists but the value is not an array or `null`.
    ///   Also rethrows any error thrown by `transform`.
    /// - Complexity: O(*M* + *N*) where *M* is the length of `array` and *N* is the length of the result.
    func flatMapArrayOrNil<S: Sequence>(_ key: String, _ transform: (JSON) throws -> S) throws -> [S.Iterator.Element]? {
        return try getArrayOrNil(key, { try JSON.flatMap($0, transform) })
    }
}

// MARK: -

internal func getRequired(_ dict: JSONObject, key: String, type: JSONError.JSONType) throws -> JSON {
    guard let value = dict[key] else { throw JSONError.missingOrInvalidType(path: key, expected: .required(type), actual: nil) }
    return value
}

internal func getRequired(_ ary: JSONArray, index: Int, type: JSONError.JSONType) throws -> JSON {
    guard let value = ary[safe: index] else { throw JSONError.missingOrInvalidType(path: "[\(index)]", expected: .required(type), actual: nil) }
    return value
}

@inline(__always)
internal func scoped<T>(_ key: String, _ f: () throws -> T) rethrows -> T {
    do {
        return try f()
    } catch let error as JSONError {
        throw error.withPrefix(key)
    }
}

@inline(__always)
internal func scoped<T>(_ index: Int, _ f: () throws -> T) rethrows -> T {
    do {
        return try f()
    } catch let error as JSONError {
        throw error.withPrefix("[\(index)]")
    }
}

internal extension ContiguousArray {
    subscript(safe index: Int) -> Element? {
        guard index >= startIndex && index < endIndex else { return nil }
        return self[index]
    }
}
