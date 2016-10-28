//
//  JSONSubscripting.swift
//  Freddy
//
//  Created by Zachary Waldowski on 8/15/15.
//  Copyright © 2015 Big Nerd Ranch. All rights reserved.
//

// MARK: JSONPathType

/// A protocol used to define a path within an instance of `JSON` that leads to some desired value.
///
/// A custom type, such as a `RawRepresentable` enum, may be made to conform to `JSONPathType`
/// and used with the subscript APIs.
public protocol JSONPathType {
    /// Use `self` to key into a `dictionary`.
    ///
    /// Unlike Swift dictionaries, failing to find a value for a key should throw
    /// an error rather than convert to `nil`.
    ///
    /// Upon failure, implementers should throw an error from `JSON.Error`.
    func value(in dictionary: [String : JSON]) throws -> JSON

    /// Use `self` to index into an `array`.
    ///
    /// Unlike Swift arrays, attempting to index outside the collection's bounds
    /// should throw an error rather than crash.
    ///
    /// Upon failure, implementers should throw an error from `JSON.Error`.
    func value(in array: [JSON]) throws -> JSON
}

extension JSONPathType {

    /// The default behavior for keying into a dictionary is to throw
    /// `JSON.Error.UnexpectedSubscript`.
    public func value(in dictionary: [String : JSON]) throws -> JSON {
        throw JSON.Error.unexpectedSubscript(type: Self.self)
    }

    /// The default behavior for indexing into an array is to throw
    /// `JSON.Error.UnexpectedSubscript`.
    public func value(in array: [JSON]) throws -> JSON {
        throw JSON.Error.unexpectedSubscript(type: Self.self)
    }

}

extension String: JSONPathType {

    /// A method used to retrieve a value from a given dictionary for a specific key.
    /// - parameter dictionary: A `Dictionary` with `String` keys and `JSON` values.
    /// - throws: `.KeyNotFound` with an associated value of `self`, where `self` is a `String`, 
    ///           should the key not be present within the `JSON`.
    /// - returns: The `JSON` value associated with the given key.
    public func value(in dictionary: [String : JSON]) throws -> JSON {
        guard let next = dictionary[self] else {
            throw JSON.Error.keyNotFound(key: self)
        }
        return next
    }

}

extension Int: JSONPathType {

    /// A method used to retrieve a value from a given array for a specific index.
    /// - parameter array: An `Array` of `JSON`.
    /// - throws: `.IndexOutOfBounds` with an associated value of `self`, where `self` is an `Int`, 
    ///           should the index not be within the valid range for the array of `JSON`.
    /// - returns: The `JSON` value found at the given index.
    public func value(in array: [JSON]) throws -> JSON {
        guard case array.indices = self else {
            throw JSON.Error.indexOutOfBounds(index: self)
        }
        return array[self]
    }

}

// MARK: - Subscripting core

private extension JSON {

    enum SubscriptError: Swift.Error {
        case subscriptIntoNull(JSONPathType)
    }

    func value(for pathFragment: JSONPathType, detectingNull: Bool) throws -> JSON {
        switch self {
        case .null where detectingNull:
            throw SubscriptError.subscriptIntoNull(pathFragment)
        case let .dictionary(dict):
            return try pathFragment.value(in: dict)
        case let .array(array):
            return try pathFragment.value(in: array)
        default:
            throw Error.unexpectedSubscript(type: type(of: pathFragment))
        }
    }

    func value(at path: [JSONPathType], detectingNull: Bool = false) throws -> JSON {
        var result = self
        for pathFragment in path {
            result = try result.value(for: pathFragment, detectingNull: detectingNull)
        }
        return result
    }

}

// MARK: - Subscripting operator

extension JSON {

    public subscript(key: String) -> JSON? {
        return try? value(for: key, detectingNull: false)
    }

    public subscript(index: Int) -> JSON? {
        return try? value(for: index, detectingNull: false)
    }

}

// MARK: - Simple member unpacking

extension JSON {

    /// Attempts to decode into the returning type from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`.
    /// - parameter type: If the context this method is called from does not
    ///                   make the return type clear, pass a type implementing `JSONDecodable`
    ///                   to disambiguate the type to decode with.
    /// - returns: An initialized member from the inner JSON.
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A given `String` key does not exist inside a
    ///     descendant `JSON` dictionary.
    ///   * `IndexOutOfBounds`: A given `Int` index is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A given subscript cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of
    ///     the `JSON` instance does not match `Decoded`.
    public func decode<Decoded: JSONDecodable>(at path: JSONPathType..., type: Decoded.Type = Decoded.self) throws -> Decoded {
        return try Decoded(json: value(at: path))
    }

    /// Retrieves a `Double` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - returns: A floating-point `Double`
    /// - throws: One of the `JSON.Error` cases thrown by `decode(at:type:)`.
    /// - seealso: `JSON.decode(at:type:)`
    public func getDouble(at path: JSONPathType...) throws -> Double {
        return try Double(json: value(at: path))
    }

    /// Retrieves an `Int` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - returns: A numeric `Int`
    /// - throws: One of the `JSON.Error` cases thrown by `decode(at:type:)`.
    /// - seealso: `JSON.decode(at:type:)`
    public func getInt(at path: JSONPathType...) throws -> Int {
        return try Int(json: value(at: path))
    }

    /// Retrieves a `String` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - returns: A textual `String`
    /// - throws: One of the `JSON.Error` cases thrown by `decode(at:type:)`.
    /// - seealso: `JSON.decode(at:type:)`
    public func getString(at path: JSONPathType...) throws -> String {
        return try String(json: value(at: path))
    }

    /// Retrieves a `Bool` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - returns: A truthy `Bool`
    /// - throws: One of the `JSON.Error` cases thrown by `decode(at:type:)`.
    /// - seealso: `JSON.decode(at:type:)`
    public func getBool(at path: JSONPathType...) throws -> Bool {
        return try Bool(json: value(at: path))
    }

    /// Retrieves a `[JSON]` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - returns: An `Array` of `JSON` elements
    /// - throws: One of the `JSON.Error` cases thrown by `decode(at:type:)`.
    /// - seealso: `JSON.decode(at:type:)`
    public func getArray(at path: JSONPathType...) throws -> [JSON] {
        return try JSON.getArray(from: value(at: path))
    }

    /// Attempts to decode many values from a descendant JSON array at a path
    /// into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter type: If the context this method is called from does not
    ///                   make the return type clear, pass a type implementing `JSONDecodable`
    ///                   to disambiguate the type to decode with.
    /// - returns: An `Array` of decoded elements
    /// - throws: One of the `JSON.Error` cases thrown by `decode(at:type:)`, or
    ///           any error that arises from decoding the contained values.
    /// - seealso: `JSON.decode(at:type:)`
    public func decodedArray<Decoded: JSONDecodable>(at path: JSONPathType..., type: Decoded.Type = Decoded.self) throws -> [Decoded] {
        return try JSON.decodedArray(from: value(at: path))
    }

    /// Retrieves a `[String: JSON]` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - returns: An `Dictionary` of `String` mapping to `JSON` elements
    /// - throws: One of the `JSON.Error` cases thrown by `decode(at:type:)`.
    /// - seealso: `JSON.decode(at:type:)`
    public func getDictionary(at path: JSONPathType...) throws -> [String: JSON] {
        return try JSON.getDictionary(from: value(at: path))
    }
    
    /// Attempts to decode many values from a descendant JSON object at a path
    /// into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter type: If the context this method is called from does not
    ///                   make the return type clear, pass a type implementing `JSONDecodable`
    ///                   to disambiguate the value type to decode with.
    /// - returns: A `Dictionary` of `String` keys and decoded values.
    /// - throws: One of the `JSON.Error` cases thrown by `decode(at:type:)` or
    ///           any error that arises from decoding the contained values.
    /// - seealso: `JSON.decode(at:type:)`
    public func decodedDictionary<Decoded: JSONDecodable>(at path: JSONPathType..., type: Decoded.Type = Decoded.self) throws -> [String: Decoded] {
        return try JSON.decodedDictionary(from: value(at: path))
    }

}

// MARK: - NotFound-Or-Null-to-Optional unpacking

extension JSON {
    
    /// An `OptionSetType` used to represent the different options available for subscripting `JSON` with `null` values or missing keys.
    /// * `.NullBecomesNil` - Treat `null` values as `nil`.
    /// * `.MissingKeyBecomesNil` - Treat missing keys as `nil`.
    public struct SubscriptingOptions: OptionSet {
        public let rawValue: Int
        public init(rawValue: Int) {
            self.rawValue = rawValue
        }
        
        /// Treat `null` values as `nil`.
        public static let NullBecomesNil = SubscriptingOptions(rawValue: 1 << 0)
        /// Treat missing keys as `nil`.
        public static let MissingKeyBecomesNil = SubscriptingOptions(rawValue: 1 << 1)
    }
    
    fileprivate func mapOptional<Value>(at path: [JSONPathType], alongPath options: SubscriptingOptions, transform: (JSON) throws -> Value) throws -> Value? {
        let detectNull = options.contains(.NullBecomesNil)
        let detectNotFound = options.contains(.MissingKeyBecomesNil)
        var json: JSON?
        do {
            json = try value(at: path, detectingNull: detectNull)
            return try json.map(transform)
        } catch Error.indexOutOfBounds where detectNotFound {
            return nil
        } catch Error.keyNotFound where detectNotFound {
            return nil
        } catch Error.valueNotConvertible where detectNull && json == .null {
            return nil
        } catch SubscriptError.subscriptIntoNull where detectNull {
            return nil
        }
    }
}

extension JSON {

    /// Decodes a `JSON` instance if it is not `.Null`, throws otherwise.
    /// - parameter json: An instance of `JSON`.
    /// - returns: An instance of some type that conforms to `JSONDecodable`.
    /// - throws: `Error.ValueNotConvertible` if the `JSON` instance is `.Null`.
    private static func getDecoded<Decoded: JSONDecodable>(json: JSON) throws -> Decoded {
        guard json != .null else {
            throw Error.valueNotConvertible(value: json, to: Decoded.self)
        }
        return try Decoded.init(json: json)
    }
    
    /// Optionally decodes into the returning type from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter alongPath: Options that control what should be done with values that are `null` or keys that are missing.
    /// - parameter type: If the context this method is called from does not
    ///                   make the return type clear, pass a type implementing `JSONDecodable`
    ///                   to disambiguate the type to decode with.
    /// - returns: A decoded value from the inner JSON if found, or `nil`.
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    ///   * Any error that arises from decoding the value.
    public func decode<Decoded: JSONDecodable>(at path: JSONPathType..., alongPath options: SubscriptingOptions, type: Decoded.Type = Decoded.self) throws -> Decoded? {
        return try mapOptional(at: path, alongPath: options, transform: JSON.getDecoded)
    }

    /// Optionally retrieves a `Double` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`.
    /// - parameter alongPath: Options that control what should be done with values that are `null` or keys that are missing.
    /// - returns: A `Double` if a value could be found, otherwise `nil`.
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    public func getDouble(at path: JSONPathType..., alongPath options: SubscriptingOptions) throws -> Double? {
        return try mapOptional(at: path, alongPath: options, transform: Double.init)
    }

    /// Optionally retrieves a `Int` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter alongPath: Options that control what should be done with values that are `null` or keys that are missing.
    /// - returns: A numeric `Int` if a value could be found, otherwise `nil`.
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    public func getInt(at path: JSONPathType..., alongPath options: SubscriptingOptions) throws -> Int? {
        return try mapOptional(at: path, alongPath: options, transform: Int.init)
    }

    /// Optionally retrieves a `String` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter alongPath: Options that control what should be done with values that are `null` or keys that are missing.
    /// - returns: A text `String` if a value could be found, otherwise `nil`.
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    public func getString(at path: JSONPathType..., alongPath options: SubscriptingOptions) throws -> String? {
        return try mapOptional(at: path, alongPath: options, transform: String.init)
    }

    /// Optionally retrieves a `Bool` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter alongPath: Options that control what should be done with values that are `null` or keys that are missing.
    /// - returns: A truthy `Bool` if a value could be found, otherwise `nil`.
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    public func getBool(at path: JSONPathType..., alongPath options: SubscriptingOptions) throws -> Bool? {
        return try mapOptional(at: path, alongPath: options, transform: Bool.init)
    }

    /// Optionally retrieves a `[JSON]` from a path into the recieving structure.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter alongPath: Options that control what should be done with values that are `null` or keys that are missing.
    /// - returns: An `Array` of `JSON` elements if a value could be found,
    ///            otherwise `nil`.
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    public func getArray(at path: JSONPathType..., alongPath options: SubscriptingOptions) throws -> [JSON]? {
        return try mapOptional(at: path, alongPath: options, transform: JSON.getArray)
    }

    /// Optionally decodes many values from a descendant array at a path into
    /// JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter alongPath: Options that control what should be done with values that are `null` or keys that are missing.
    /// - parameter type: If the context this method is called from does not
    ///                   make the return type clear, pass a type implementing `JSONDecodable`
    ///                   to disambiguate the value type to decode with.
    /// - returns: An `Array` of decoded elements if found, otherwise `nil`.
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    ///   * Any error that arises from decoding the value.
    public func decodedArray<Decoded: JSONDecodable>(at path: JSONPathType..., alongPath options: SubscriptingOptions, type: Decoded.Type = Decoded.self) throws -> [Decoded]? {
        return try mapOptional(at: path, alongPath: options, transform: JSON.decodedArray)
    }

    /// Optionally retrieves a `[String: JSON]` from a path into the recieving
    /// structure.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter alongPath: Options that control what should be done with values that are `null` or keys that are missing.
    /// - returns: A `Dictionary` of `String` mapping to `JSON` elements if a
    ///            value could be found, otherwise `nil`.
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    public func getDictionary(at path: JSONPathType..., alongPath options: SubscriptingOptions) throws -> [String: JSON]? {
        return try mapOptional(at: path, alongPath: options, transform: JSON.getDictionary)
    }
    
    /// Optionally attempts to decode many values from a descendant object at a path
    /// into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter alongPath: Options that control what should be done with values that are `null` or keys that are missing.
    /// - parameter type: If the context this method is called from does not
    ///                   make the return type clear, pass a type implementing `JSONDecodable`
    ///                   to disambiguate the value type to decode with.
    /// - returns: A `Dictionary` of `String` mapping to decoded elements if a
    ///            value could be found, otherwise `nil`.
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    ///   * Any error that arises from decoding the value.
    public func decodedDictionary<Decoded: JSONDecodable>(at path: JSONPathType..., alongPath options: SubscriptingOptions, type: Decoded.Type = Decoded.self) throws -> [String: Decoded]? {
        return try mapOptional(at: path, alongPath: options, transform: JSON.decodedDictionary)
    }

}

// MARK: - Missing-with-fallback unpacking

extension JSON {
    
    fileprivate func mapOptional<Value>(at path: [JSONPathType], fallback: () -> Value, transform: (JSON) throws -> Value) throws -> Value {
        return try mapOptional(at: path, alongPath: .MissingKeyBecomesNil, transform: transform) ?? fallback()
    }
    
    /// Attempts to decode into the returning type from a path into
    /// JSON, or returns a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: Value to use when one is missing at the subscript.
    /// - returns: An initialized member from the inner JSON.
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `UnexpectedSubscript`: A given subscript cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of
    ///     the `JSON` instance does not match `Decoded`.
    public func decode<Decoded: JSONDecodable>(at path: JSONPathType..., or fallback: @autoclosure() -> Decoded) throws -> Decoded {
        return try mapOptional(at: path, fallback: fallback, transform: Decoded.init)
    }
    
    /// Retrieves a `Double` from a path into JSON or a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: `Double` to use when one is missing at the subscript.
    /// - returns: A floating-point `Double`
    /// - throws: One of the `JSON.Error` cases thrown by calling `mapOptional(at:fallback:transform:)`.
    public func getDouble(at path: JSONPathType..., or fallback: @autoclosure() -> Double) throws -> Double {
        return try mapOptional(at: path, fallback: fallback, transform: Double.init)
    }
    
    /// Retrieves an `Int` from a path into JSON or a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: `Int` to use when one is missing at the subscript.
    /// - returns: A numeric `Int`
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    public func getInt(at path: JSONPathType..., or fallback: @autoclosure() -> Int) throws -> Int {
        return try mapOptional(at: path, fallback: fallback, transform: Int.init)
    }
    
    /// Retrieves a `String` from a path into JSON or a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: `String` to use when one is missing at the subscript.
    /// - returns: A textual `String`
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    public func getString(at path: JSONPathType..., or fallback: @autoclosure() -> String) throws -> String {
        return try mapOptional(at: path, fallback: fallback, transform: String.init)
    }
    
    /// Retrieves a `Bool` from a path into JSON or a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: `Bool` to use when one is missing at the subscript.
    /// - returns: A truthy `Bool`
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    public func getBool(at path: JSONPathType..., or fallback: @autoclosure() -> Bool) throws -> Bool {
        return try mapOptional(at: path, fallback: fallback, transform: Bool.init)
    }
    
    /// Retrieves a `[JSON]` from a path into JSON or a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: `Array` to use when one is missing at the subscript.
    /// - returns: An `Array` of `JSON` elements
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    public func getArray(at path: JSONPathType..., or fallback: @autoclosure() -> [JSON]) throws -> [JSON] {
        return try mapOptional(at: path, fallback: fallback, transform: JSON.getArray)
    }
    
    /// Attempts to decodes many values from a desendant JSON array at a path
    /// into the recieving structure, returning a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: `Array` to use when one is missing at the subscript.
    /// - returns: An `Array` of decoded elements
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    ///   * Any error that arises from decoding the value.
    public func decodedArray<Decoded: JSONDecodable>(at path: JSONPathType..., or fallback: @autoclosure() -> [Decoded]) throws -> [Decoded] {
        return try mapOptional(at: path, fallback: fallback, transform: JSON.decodedArray)
    }
    
    /// Retrieves a `[String: JSON]` from a path into JSON or a fallback if not
    /// found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: `Dictionary` to use when one is missing at the subscript.
    /// - returns: An `Dictionary` of `String` mapping to `JSON` elements
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    public func getDictionary(at path: JSONPathType..., or fallback: @autoclosure() -> [String: JSON]) throws -> [String: JSON] {
        return try mapOptional(at: path, fallback: fallback, transform: JSON.getDictionary)
    }
    
    /// Attempts to decode many values from a descendant JSON object at a path
    /// into the receiving structure, returning a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: Value to use when one is missing at the subscript
    /// - returns: A `Dictionary` of `String` mapping to decoded elements.
    /// - throws: One of the following errors contained in `JSON.Error`:
    ///   * `KeyNotFound`: A key `path` does not exist inside a descendant
    ///     `JSON` dictionary.
    ///   * `IndexOutOfBounds`: An index `path` is outside the bounds of a
    ///     descendant `JSON` array.
    ///   * `UnexpectedSubscript`: A `path` item cannot be used with the
    ///     corresponding `JSON` value.
    ///   * `TypeNotConvertible`: The target value's type inside of the `JSON`
    ///     instance does not match the decoded value.
    ///   * Any error that arises from decoding the value.
    public func decodedDictionary<Decoded: JSONDecodable>(at path: JSONPathType..., or fallback: @autoclosure() -> [String: Decoded]) throws -> [String: Decoded] {
        return try mapOptional(at: path, fallback: fallback, transform: JSON.decodedDictionary)
    }
    
    
}
