//
//  JSONLiteralConvertible.swift
//  Freddy
//
//  Created by Zachary Waldowski on 5/11/15.
//  Copyright © 2015 Big Nerd Ranch. Licensed under MIT.
//

// MARK: - ArrayLiteralConvertible

extension JSON: ExpressibleByArrayLiteral {
    
    /// Create an instance by copying each element of the `collection` into a
    /// new `Array`.
    public init<Collection: Swift.Collection>(_ collection: Collection) where Collection.Iterator.Element == JSON {
        self = .array(Swift.Array(collection))
    }

    /// Create an instance initialized with `elements`.
    public init(arrayLiteral elements: JSON...) {
        self.init(elements)
    }
    
}

// MARK: - DictionaryLiteralConvertible

extension JSON: ExpressibleByDictionaryLiteral {

    /// Create an instance by copying each key/value pair of the `pairs` into
    /// a new `Dictionary`.
    public init<Dictionary: Sequence>(_ pairs: Dictionary) where Dictionary.Iterator.Element == (Swift.String, JSON) {
        var dictionary = Swift.Dictionary<Swift.String, JSON>(minimumCapacity: pairs.underestimatedCount)
        for (key, value) in pairs {
            dictionary[key] = value
        }
        self.init(dictionary)
    }

    /// Create an instance initialized with `pairs`.
    public init(dictionaryLiteral pairs: (Swift.String, JSON)...) {
        self.init(pairs)
    }

    /// Create an instance initialized to `dictionary`.
    public init(_ dictionary: Swift.Dictionary<Swift.String, JSON>) {
        self = .dictionary(dictionary)
    }
}

// MARK: - FloatLiteralConvertible

extension JSON: ExpressibleByFloatLiteral {
    
    /// Create an instance initialized to `Double` `value`.
    public init(_ value: Swift.Double) {
        self = .double(value)
    }
    
    /// Create a literal instance initialized to `value`.
    public init(floatLiteral value: Swift.Double) {
        self.init(value)
    }

}

// MARK: - IntegerLiteralConvertible

extension JSON: ExpressibleByIntegerLiteral {
    
    /// Create an instance initialized to `Int` by `value`.
    public init(_ value: Swift.Int) {
        self = .int(value)
    }
    
    /// Create a literal instance initialized to `value`.
    public init(integerLiteral value: Swift.Int) {
        self.init(value)
    }

}

// MARK: - StringLiteralConvertible

extension JSON: ExpressibleByStringLiteral {
    
    /// Create an instance initialized to `String` by `text`.
    public init(_ text: Swift.String) {
        self = .string(text)
    }

    /// Create a literal instance initialized to `value`.
    public init(stringLiteral value: StringLiteralType) {
        self.init(value)
    }
    
    /// Create a literal instance initialized to `value`.
    public init(extendedGraphemeClusterLiteral value: StringLiteralType) {
        self.init(value)
    }
    
    /// Create a literal instance initialized to `value`.
    public init(unicodeScalarLiteral value: StringLiteralType) {
        self.init(value)
    }
    
}

// MARK: - BooleanLiteralConvertible

extension JSON: ExpressibleByBooleanLiteral {

    /// Create an instance initialized to `Bool` by `value`.
    public init(_ value: Swift.Bool) {
        self = .bool(value)
    }

    /// Create a literal instance initialized to `value`.
    public init(booleanLiteral value: Swift.Bool) {
        self.init(value)
    }

}

// MARK: - NilLiteralConvertible

extension JSON: ExpressibleByNilLiteral {

    /// Create an instance initialized with `nil`.
    public init(nilLiteral: ()) {
        self = .null
    }

}
