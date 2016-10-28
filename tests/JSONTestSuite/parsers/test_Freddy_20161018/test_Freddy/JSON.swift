//
//  JSON.swift
//  Freddy
//
//  Created by Matthew D. Mathias on 3/17/15.
//  Copyright © 2015 Big Nerd Ranch. Licensed under MIT.
//

/// An enum to describe the structure of JSON.
public enum JSON {
    /// A case for denoting an array with an associated value of `[JSON]`
    case array([JSON])
    /// A case for denoting a dictionary with an associated value of `[Swift.String: JSON]`
    case dictionary([String: JSON])
    /// A case for denoting a double with an associated value of `Swift.Double`.
    case double(Double)
    /// A case for denoting an integer with an associated value of `Swift.Int`.
    case int(Int)
    /// A case for denoting a string with an associated value of `Swift.String`.
    case string(String)
    /// A case for denoting a boolean with an associated value of `Swift.Bool`.
    case bool(Bool)
    /// A case for denoting null.
    case null
}

// MARK: - Errors

extension JSON {

    /// An enum to encapsulate errors that may arise in working with `JSON`.
    public enum Error: Swift.Error {
        /// The `index` is out of bounds for a JSON array
        case indexOutOfBounds(index: Int)
        
        /// The `key` was not found in the JSON dictionary
        case keyNotFound(key: String)
        
        /// The JSON is not subscriptable with `type`
        case unexpectedSubscript(type: JSONPathType.Type)
        
        /// Unexpected JSON `value` was found that is not convertible `to` type 
        case valueNotConvertible(value: JSON, to: Any.Type)
        
        /// The JSON is not serializable to a `String`.
        case stringSerializationError
    }

}

// MARK: - Test Equality

/// Return `true` if `lhs` is equal to `rhs`.
public func ==(lhs: JSON, rhs: JSON) -> Bool {
    switch (lhs, rhs) {
    case (.array(let arrL), .array(let arrR)):
        return arrL == arrR
    case (.dictionary(let dictL), .dictionary(let dictR)):
        return dictL == dictR
    case (.string(let strL), .string(let strR)):
        return strL == strR
    case (.double(let dubL), .double(let dubR)):
        return dubL == dubR
    case (.double(let dubL), .int(let intR)):
        return dubL == Double(intR)
    case (.int(let intL), .int(let intR)):
        return intL == intR
    case (.int(let intL), .double(let dubR)):
        return Double(intL) == dubR
    case (.bool(let bL), .bool(let bR)):
        return bL == bR
    case (.null, .null):
        return true
    default:
        return false
    }
}

extension JSON: Equatable {}

// MARK: - Printing

extension JSON: CustomStringConvertible {

    /// A textual representation of `self`.
    public var description: Swift.String {
        switch self {
        case .array(let arr):       return String(describing: arr)
        case .dictionary(let dict): return String(describing: dict)
        case .string(let string):   return string
        case .double(let double):   return String(describing: double)
        case .int(let int):         return String(describing: int)
        case .bool(let bool):       return String(describing: bool)
        case .null:                 return "null"
        }
    }

}
