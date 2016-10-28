//
//  JSONDecodable.swift
//  Freddy
//
//  Created by Matthew D. Mathias on 3/24/15.
//  Copyright © 2015 Big Nerd Ranch. Licensed under MIT.
//

/// A protocol to provide functionality for creating a model object with a `JSON`
/// value.
public protocol JSONDecodable {
    
    /// Creates an instance of the model with a `JSON` instance.
    /// - parameter json: An instance of a `JSON` value from which to
    ///             construct an instance of the implementing type.
    /// - throws: Any `JSON.Error` for errors derived from inspecting the
    ///           `JSON` value, or any other error involved in decoding.
    init(json: JSON) throws
    
}

extension Double: JSONDecodable {
    
    /// An initializer to create an instance of `Double` from a `JSON` value.
    /// - parameter json: An instance of `JSON`.
    /// - throws: The initializer will throw an instance of `JSON.Error` if 
    ///           an instance of `Double` cannot be created from the `JSON` value that was
    ///           passed to this initializer.
    public init(json: JSON) throws {
        switch json {
        case let .Double(double):
            self = double
        case let .Int(int):
            self = Swift.Double(int)
        default:
            throw JSON.Error.ValueNotConvertible(value: json, to: Swift.Double)
        }
    }
    
}

extension Int: JSONDecodable {
    
    /// An initializer to create an instance of `Int` from a `JSON` value.
    /// - parameter json: An instance of `JSON`.
    /// - throws: The initializer will throw an instance of `JSON.Error` if
    ///           an instance of `Int` cannot be created from the `JSON` value that was
    ///           passed to this initializer.
    public init(json: JSON) throws {
        switch json {
        case let .Double(double) where double <= Double(Swift.Int.max):
            self = Swift.Int(double)
        case let .Int(int):
            self = int
        default:
            throw JSON.Error.ValueNotConvertible(value: json, to: Swift.Int)
        }
    }
    
}

extension String: JSONDecodable {
    
    /// An initializer to create an instance of `String` from a `JSON` value.
    /// - parameter json: An instance of `JSON`.
    /// - throws: The initializer will throw an instance of `JSON.Error` if
    ///           an instance of `String` cannot be created from the `JSON` value that was
    ///           passed to this initializer.
    public init(json: JSON) throws {
        guard case let .String(string) = json else {
            throw JSON.Error.ValueNotConvertible(value: json, to: Swift.String)
        }
        self = string
    }
    
}

extension Bool: JSONDecodable {
    
    /// An initializer to create an instance of `Bool` from a `JSON` value.
    /// - parameter json: An instance of `JSON`.
    /// - throws: The initializer will throw an instance of `JSON.Error` if
    ///           an instance of `Bool` cannot be created from the `JSON` value that was
    ///           passed to this initializer.
    public init(json: JSON) throws {
        guard case let .Bool(bool) = json else {
            throw JSON.Error.ValueNotConvertible(value: json, to: Swift.Bool)
        }
        self = bool
    }
    
}

extension RawRepresentable where RawValue: JSONDecodable {

    /// An initializer to create an instance of `RawRepresentable` from a `JSON` value.
    /// - parameter json: An instance of `JSON`.
    /// - throws: The initializer will throw an instance of `JSON.Error` if
    ///           an instance of `RawRepresentable` cannot be created from the `JSON` value that was
    ///           passed to this initializer.
    public init(json: JSON) throws {
        let raw = try json.decode(type: RawValue.self)
        guard let value = Self(rawValue: raw) else {
            throw JSON.Error.ValueNotConvertible(value: json, to: Self.self)
        }
        self = value
    }
}

internal extension JSON {

    /// Retrieves a `[JSON]` from the JSON.
    /// - returns: An `Array` of `JSON` elements
    /// - throws: Any of the `JSON.Error` cases thrown by `decode(type:)`.
    /// - seealso: `JSON.decode(_:type:)`
    static func getArray(json: JSON) throws -> [JSON] {
        // Ideally should be expressed as a conditional protocol implementation on Swift.Array.
        guard case let .Array(array) = json else {
            throw Error.ValueNotConvertible(value: json, to: Swift.Array<JSON>)
        }
        return array
    }
    
    /// Retrieves a `[String: JSON]` from the JSON.
    /// - returns: An `Dictionary` of `String` mapping to `JSON` elements
    /// - throws: Any of the `JSON.Error` cases thrown by `decode(type:)`.
    /// - seealso: `JSON.decode(_:type:)`
    static func getDictionary(json: JSON) throws -> [Swift.String: JSON] {
        // Ideally should be expressed as a conditional protocol implementation on Swift.Dictionary.
        guard case let .Dictionary(dictionary) = json else {
            throw Error.ValueNotConvertible(value: json, to: Swift.Dictionary<Swift.String, JSON>)
        }
        return dictionary
    }
    
    /// Attempts to decode many values from a descendant JSON array at a path
    /// into JSON.
    /// - parameter type: If the context this method is called from does not
    ///   make the return type clear, pass a type implementing `JSONDecodable`
    ///   to disambiguate the type to decode with.
    /// - returns: An `Array` of decoded elements
    /// - throws: Any of the `JSON.Error` cases thrown by `decode(type:)`, as
    ///   well as any error that arises from decoding the contained values.
    /// - seealso: `JSON.decode(_:type:)`
    static func getArrayOf<Decoded: JSONDecodable>(json: JSON) throws -> [Decoded] {
        // Ideally should be expressed as a conditional protocol implementation on Swift.Dictionary.
        // This implementation also doesn't do the `type = Type.self` trick.
        return try getArray(json).map(Decoded.init)
    }
    
}
