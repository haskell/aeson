//
//  JSON.swift
//  PMJSON
//
//  Created by Kevin Ballard on 10/8/15.
//  Copyright © 2016 Postmates.
//
//  Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
//  http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
//  <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
//  option. This file may not be copied, modified, or distributed
//  except according to those terms.
//

/// A single JSON-compatible value.
public enum JSON {
    /// The null value.
    case null
    /// A boolean.
    case bool(Bool)
    /// A string.
    case string(String)
    /// A 64-bit integer.
    case int64(Int64)
    /// A number.
    /// When decoding, any integer that doesn't fit in 64 bits and any floating-point number
    /// is decoded as a `Double`.
    case double(Double)
    /// An object.
    case object(JSONObject)
    /// An array.
    case array(JSONArray)
    
    /// Initializes `self` as a boolean with the value `bool`.
    public init(_ bool: Bool) {
        self = .bool(bool)
    }
    /// Initializes `self` as a string with the value `str`.
    public init(_ str: String) {
        self = .string(str)
    }
    /// Initializes `self` as a 64-bit integer with the value `i`.
    public init(_ i: Int64) {
        self = .int64(i)
    }
    /// Initializes `self` as a double with the value `d`.
    public init(_ d: Double) {
        self = .double(d)
    }
    /// Initializes `self` as an object with the value `obj`.
    public init(_ obj: JSONObject) {
        self = .object(obj)
    }
    /// Initializes `self` as an array with the value `ary`.
    public init(_ ary: JSONArray) {
        self = .array(ary)
    }
}

// Convenience conversions.
public extension JSON {
    /// Initializes `self` as a 64-bit integer with the value `i`.
    public init(_ i: Int) {
        self = .int64(Int64(i))
    }
    
    /// Initializes `self` as an array with the contents of the sequence `seq`.
    public init<S: Sequence>(_ seq: S) where S.Iterator.Element == JSON {
        self = .array(JSONArray(seq))
    }
    
    /// Initializes `self` as an array with the contents of the sequence `seq`.
    public init<S: Sequence>(_ seq: S) where S.Iterator.Element == JSONObject {
        self = .array(JSONArray(seq.lazy.map(JSON.init)))
    }
    
    /// Initializes `self` as an array with the contents of the sequence `seq`.
    public init<S: Sequence>(_ seq: S) where S.Iterator.Element == JSONArray {
        self = .array(JSONArray(seq.lazy.map(JSON.init)))
    }
}

public typealias JSONArray = ContiguousArray<JSON>

extension JSON: Equatable {
    public static func ==(lhs: JSON, rhs: JSON) -> Bool {
        switch (lhs, rhs) {
        case (.null, .null): return true
        case (.bool(let a), .bool(let b)): return a == b
        case (.string(let a), .string(let b)): return a == b
        case (.int64(let a), .int64(let b)): return a == b
        case (.double(let a), .double(let b)): return a == b
        case (.int64(let a), .double(let b)): return Double(a) == b
        case (.double(let a), .int64(let b)): return a == Double(b)
        case (.object(let a), .object(let b)): return a == b
        case (.array(let a), .array(let b)): return a == b
        default: return false
        }
    }
}

extension JSON: TextOutputStreamable, CustomStringConvertible, CustomDebugStringConvertible {
    public func write<Target : TextOutputStream>(to target: inout Target) {
        JSON.encode(self, to: &target)
    }
    
    public var description: String {
        return JSON.encodeAsString(self)
    }
    
    public var debugDescription: String {
        let desc = JSON.encodeAsString(self)
        return "JSON(\(desc))"
    }
}

extension JSON: ExpressibleByIntegerLiteral, ExpressibleByFloatLiteral, ExpressibleByBooleanLiteral, ExpressibleByNilLiteral {
    public init(integerLiteral value: Int64) {
        self = .int64(value)
    }
    
    public init(floatLiteral value: Double) {
        self = .double(value)
    }
    
    public init(booleanLiteral value: Bool) {
        self = .bool(value)
    }
    
    public init(nilLiteral: ()) {
        self = .null
    }
}

extension JSON: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        self = .string(value)
    }
    
    public init(extendedGraphemeClusterLiteral value: String) {
        self = .string(value)
    }
    
    public init(unicodeScalarLiteral value: String) {
        self = .string(value)
    }
}

extension JSON: ExpressibleByArrayLiteral, ExpressibleByDictionaryLiteral {
    public init(arrayLiteral elements: JSON...) {
        self = .array(JSONArray(elements))
    }
    
    public init(dictionaryLiteral elements: (String, JSON)...) {
        self = .object(JSONObject(elements))
    }
}

extension JSON: CustomReflectable {
    public var customMirror: Mirror {
        switch self {
        case .null, .bool, .string, .int64, .double: return Mirror(self, children: [])
        case .object(let obj):
            let children: LazyMapCollection<JSONObject, Mirror.Child> = obj.lazy.map({ ($0, $1) })
            return Mirror(self, children: children, displayStyle: .dictionary)
        case .array(let ary):
            return Mirror(self, unlabeledChildren: ary, displayStyle: .collection)
        }
    }
}
