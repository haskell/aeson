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
    case Array([JSON])
    /// A case for denoting a dictionary with an associated value of `[Swift.String: JSON]`
    case Dictionary([Swift.String: JSON])
    /// A case for denoting a double with an associated value of `Swift.Double`.
    case Double(Swift.Double)
    /// A case for denoting an integer with an associated value of `Swift.Int`.
    case Int(Swift.Int)
    /// A case for denoting a string with an associated value of `Swift.String`.
    case String(Swift.String)
    /// A case for denoting a boolean with an associated value of `Swift.Bool`.
    case Bool(Swift.Bool)
    /// A case for denoting null.
    case Null
}

// MARK: - Errors

extension JSON {

    /// An enum to encapsulate errors that may arise in working with `JSON`.
    public enum Error: ErrorType {
        /// The `index` is out of bounds for a JSON array
        case IndexOutOfBounds(index: Swift.Int)
        
        /// The `key` was not found in the JSON dictionary
        case KeyNotFound(key: Swift.String)
        
        /// The JSON is not subscriptable with `type`
        case UnexpectedSubscript(type: JSONPathType.Type)
        
        /// Unexpected JSON `value` was found that is not convertible `to` type 
        case ValueNotConvertible(value: JSON, to: Any.Type)
    }

}

// MARK: - Test Equality

/// Return `true` if `lhs` is equal to `rhs`.
public func ==(lhs: JSON, rhs: JSON) -> Bool {
    switch (lhs, rhs) {
    case (.Array(let arrL), .Array(let arrR)):
        return arrL == arrR
    case (.Dictionary(let dictL), .Dictionary(let dictR)):
        return dictL == dictR
    case (.String(let strL), .String(let strR)):
        return strL == strR
    case (.Double(let dubL), .Double(let dubR)):
        return dubL == dubR
    case (.Double(let dubL), .Int(let intR)):
        return dubL == Double(intR)
    case (.Int(let intL), .Int(let intR)):
        return intL == intR
    case (.Int(let intL), .Double(let dubR)):
        return Double(intL) == dubR
    case (.Bool(let bL), .Bool(let bR)):
        return bL == bR
    case (.Null, .Null):
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
        case .Array(let arr):       return Swift.String(arr)
        case .Dictionary(let dict): return Swift.String(dict)
        case .String(let string):   return string
        case .Double(let double):   return Swift.String(double)
        case .Int(let int):         return Swift.String(int)
        case .Bool(let bool):       return Swift.String(bool)
        case .Null:                 return "null"
        }
    }

}
