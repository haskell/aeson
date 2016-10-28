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
        if case let .double(double) = json {
            self = double
        } else if case let .int(int) = json {
            self = Double(int)
        } else if case let .string(string) = json, let s = Double(string) {
            self = s
        } else {
            throw JSON.Error.valueNotConvertible(value: json, to: Double.self)
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

        if case let .double(double) = json, double <= Double(Int.max) {
            self = Int(double)
        } else if case let .int(int) = json {
            self = int
        } else if case let .string(string) = json, let int = Int(string) {
            self = int
        } else if case let .string(string) = json,
            let double = Double(string),
            let decimalSeparator = string.characters.index(of: "."),
            let int = Int(String(string.characters.prefix(upTo: decimalSeparator))),
            double == Double(int) {
            self = int
        } else {
            throw JSON.Error.valueNotConvertible(value: json, to: Int.self)
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
        switch json {
        case let .string(string):
            self = string
        case let .int(int):
            self = String(int)
        case let .bool(bool):
            self = String(bool)
        case let .double(double):
            self = String(double)
        default:
            throw JSON.Error.valueNotConvertible(value: json, to: String.self)
        }
    }
    
}

extension Bool: JSONDecodable {
    
    /// An initializer to create an instance of `Bool` from a `JSON` value.
    /// - parameter json: An instance of `JSON`.
    /// - throws: The initializer will throw an instance of `JSON.Error` if
    ///           an instance of `Bool` cannot be created from the `JSON` value that was
    ///           passed to this initializer.
    public init(json: JSON) throws {
        guard case let .bool(bool) = json else {
            throw JSON.Error.valueNotConvertible(value: json, to: Bool.self)
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
            throw JSON.Error.valueNotConvertible(value: json, to: Self.self)
        }
        self = value
    }
}

internal extension JSON {

    /// Retrieves a `[JSON]` from the JSON.
    /// - parameter: A `JSON` to be used to create the returned `Array`.
    /// - returns: An `Array` of `JSON` elements
    /// - throws: Any of the `JSON.Error` cases thrown by `decode(type:)`.
    /// - seealso: `JSON.decode(_:type:)`
    static func getArray(from json: JSON) throws -> [JSON] {
        // Ideally should be expressed as a conditional protocol implementation on Swift.Array.
        guard case let .array(array) = json else {
            throw Error.valueNotConvertible(value: json, to: Swift.Array<JSON>)
        }
        return array
    }
    
    /// Retrieves a `[String: JSON]` from the JSON.
    /// - parameter: A `JSON` to be used to create the returned `Dictionary`.
    /// - returns: An `Dictionary` of `String` mapping to `JSON` elements
    /// - throws: Any of the `JSON.Error` cases thrown by `decode(type:)`.
    /// - seealso: `JSON.decode(_:type:)`
    static func getDictionary(from json: JSON) throws -> [String: JSON] {
        // Ideally should be expressed as a conditional protocol implementation on Swift.Dictionary.
        guard case let .dictionary(dictionary) = json else {
            throw Error.valueNotConvertible(value: json, to: Swift.Dictionary<String, JSON>)
        }
        return dictionary
    }
    
    /// Attempts to decode many values from a descendant JSON array at a path
    /// into JSON.
    /// - parameter json: A `JSON` to be used to create the returned `Array` of some type conforming to `JSONDecodable`.
    /// - returns: An `Array` of `Decoded` elements.
    /// - throws: Any of the `JSON.Error` cases thrown by `decode(type:)`, as
    ///   well as any error that arises from decoding the contained values.
    /// - seealso: `JSON.decode(_:type:)`
    static func decodedArray<Decoded: JSONDecodable>(from json: JSON) throws -> [Decoded] {
        // Ideally should be expressed as a conditional protocol implementation on Swift.Dictionary.
        // This implementation also doesn't do the `type = Type.self` trick.
        return try getArray(from: json).map(Decoded.init)
    }
    
    /// Attempts to decode many values from a descendant JSON object at a path
    /// into JSON.
    /// - parameter json: A `JSON` to be used to create the returned `Dictionary` of some type conforming to `JSONDecodable`.
    /// - returns: A `Dictionary` of string keys and `Decoded` values.
    /// - throws: One of the `JSON.Error` cases thrown by `decode(_:type:)` or
    ///           any error that arises from decoding the contained values.
    /// - seealso: `JSON.decode(_:type:)`
    static func decodedDictionary<Decoded: JSONDecodable>(from json: JSON) throws -> [Swift.String: Decoded] {
        guard case let .dictionary(dictionary) = json else {
            throw Error.valueNotConvertible(value: json, to: Swift.Dictionary<String, Decoded>)
        }
        var decodedDictionary = Swift.Dictionary<String, Decoded>(minimumCapacity: dictionary.count)
        for (key, value) in dictionary {
            decodedDictionary[key] = try Decoded(json: value)
        }
        return decodedDictionary
    }
    
}
