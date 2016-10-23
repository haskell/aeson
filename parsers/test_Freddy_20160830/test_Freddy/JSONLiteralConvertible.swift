//
//  JSONLiteralConvertible.swift
//  Freddy
//
//  Created by Zachary Waldowski on 5/11/15.
//  Copyright © 2015 Big Nerd Ranch. Licensed under MIT.
//

// MARK: - ArrayLiteralConvertible

extension JSON: ArrayLiteralConvertible {
    
    /// Create an instance by copying each element of the `collection` into a
    /// new `Array`.
    public init<Collection: CollectionType where Collection.Generator.Element == JSON>(_ collection: Collection) {
        self = .Array(Swift.Array(collection))
    }

    /// Create an instance initialized with `elements`.
    public init(arrayLiteral elements: JSON...) {
        self.init(elements)
    }
    
}

// MARK: - DictionaryLiteralConvertible

extension JSON: DictionaryLiteralConvertible {
    
    /// Create an instance by copying each key/value pair of the `pairs` into
    /// a new `Dictionary`.
    public init<Dictionary: SequenceType where Dictionary.Generator.Element == (Swift.String, JSON)>(_ pairs: Dictionary) {
        var dictionary = Swift.Dictionary<Swift.String, JSON>(minimumCapacity: pairs.underestimateCount())
        for (key, value) in pairs {
            dictionary[key] = value
        }
        self = .Dictionary(dictionary)
    }
    
    /// Create an instance initialized with `pairs`.
    public init(dictionaryLiteral pairs: (Swift.String, JSON)...) {
        self.init(pairs)
    }

}

// MARK: - FloatLiteralConvertible

extension JSON: FloatLiteralConvertible {
    
    /// Create an instance initialized to `Double` `value`.
    public init(_ value: Swift.Double) {
        self = .Double(value)
    }
    
    /// Create a literal instance initialized to `value`.
    public init(floatLiteral value: Swift.Double) {
        self.init(value)
    }

}

// MARK: - IntegerLiteralConvertible

extension JSON: IntegerLiteralConvertible {
    
    /// Create an instance initialized to `Int` by `value`.
    public init(_ value: Swift.Int) {
        self = .Int(value)
    }
    
    /// Create a literal instance initialized to `value`.
    public init(integerLiteral value: Swift.Int) {
        self.init(value)
    }

}

// MARK: - StringLiteralConvertible

extension JSON: StringLiteralConvertible {
    
    /// Create an instance initialized to `String` by `text`.
    public init(_ text: Swift.String) {
        self = .String(text)
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

extension JSON: BooleanLiteralConvertible {

    /// Create an instance initialized to `Bool` by `value`.
    public init(_ value: Swift.Bool) {
        self = .Bool(value)
    }

    /// Create a literal instance initialized to `value`.
    public init(booleanLiteral value: Swift.Bool) {
        self.init(value)
    }

}

// MARK: - NilLiteralConvertible

extension JSON: NilLiteralConvertible {

    /// Create an instance initialized with `nil`.
    public init(nilLiteral: ()) {
        self = .Null
    }

}
