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
    func valueInDictionary(dictionary: [Swift.String : JSON]) throws -> JSON

    /// Use `self` to index into an `array`.
    ///
    /// Unlike Swift array, attempting to index outside the collection's bounds
    /// should throw an error rather than crash.
    ///
    /// Upon failure, implementers should throw an error from `JSON.Error`.
    func valueInArray(array: [JSON]) throws -> JSON
}

extension JSONPathType {

    /// The default behavior for keying into a dictionary is to throw
    /// `JSON.Error.UnexpectedSubscript`.
    public func valueInDictionary(dictionary: [Swift.String : JSON]) throws -> JSON {
        throw JSON.Error.UnexpectedSubscript(type: Self.self)
    }

    /// The default behavior for indexing into an array is to throw
    /// `JSON.Error.UnexpectedSubscript`.
    public func valueInArray(array: [JSON]) throws -> JSON {
        throw JSON.Error.UnexpectedSubscript(type: Self.self)
    }

}

extension String: JSONPathType {

    /// A method used to retrieve a value from a given dictionary for a specific key.
    /// - throws: `.KeyNotFound` with an associated value of `self`, where `self` is a `String`, 
    ///           should the key not be present within the `JSON`.
    /// - returns: The `JSON` value associated with the given key.
    public func valueInDictionary(dictionary: [Swift.String : JSON]) throws -> JSON {
        guard let next = dictionary[self] else {
            throw JSON.Error.KeyNotFound(key: self)
        }
        return next
    }

}

extension Int: JSONPathType {

    /// A method used to retrieve a value from a given array for a specific index.
    /// - throws: `.IndexOutOfBounds` with an associated value of `self`, where `self` is an `Int`, 
    ///           should the index not be within the valid range for the array of `JSON`.
    /// - returns: The `JSON` value found at the given index.
    public func valueInArray(array: [JSON]) throws -> JSON {
        guard case array.indices = self else {
            throw JSON.Error.IndexOutOfBounds(index: self)
        }
        return array[self]
    }

}

// MARK: - Subscripting core

private extension JSON {

    enum SubscriptError: ErrorType {
        case SubscriptIntoNull(JSONPathType)
    }

    func valueForPathFragment(fragment: JSONPathType, detectNull: Swift.Bool) throws -> JSON {
        switch self {
        case .Null where detectNull:
            throw SubscriptError.SubscriptIntoNull(fragment)
        case let .Dictionary(dict):
            return try fragment.valueInDictionary(dict)
        case let .Array(array):
            return try fragment.valueInArray(array)
        default:
            throw Error.UnexpectedSubscript(type: fragment.dynamicType)
        }
    }

    func valueAtPath(path: [JSONPathType], detectNull: Swift.Bool = false) throws -> JSON {
        var result = self
        for fragment in path {
            result = try result.valueForPathFragment(fragment, detectNull: detectNull)
        }
        return result
    }

}

// MARK: - Subscripting operator

extension JSON {

    public subscript(key: Swift.String) -> JSON? {
        return try? valueForPathFragment(key, detectNull: false)
    }

    public subscript(index: Swift.Int) -> JSON? {
        return try? valueForPathFragment(index, detectNull: false)
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
    public func decode<Decoded: JSONDecodable>(path: JSONPathType..., type: Decoded.Type = Decoded.self) throws -> Decoded {
        return try Decoded(json: valueAtPath(path))
    }

    /// Retrieves a `Double` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - returns: A floating-point `Double`
    /// - throws: One of the `JSON.Error` cases thrown by `decode(_:type:)`.
    /// - seealso: `JSON.decode(_:type:)`
    public func double(path: JSONPathType...) throws -> Swift.Double {
        return try Swift.Double(json: valueAtPath(path))
    }

    /// Retrieves an `Int` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - returns: A numeric `Int`
    /// - throws: One of the `JSON.Error` cases thrown by `decode(_:type:)`.
    /// - seealso: `JSON.decode(_:type:)`
    public func int(path: JSONPathType...) throws -> Swift.Int {
        return try Swift.Int(json: valueAtPath(path))
    }

    /// Retrieves a `String` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - returns: A textual `String`
    /// - throws: One of the `JSON.Error` cases thrown by `decode(_:type:)`.
    /// - seealso: `JSON.decode(_:type:)`
    public func string(path: JSONPathType...) throws -> Swift.String {
        return try Swift.String(json: valueAtPath(path))
    }

    /// Retrieves a `Bool` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - returns: A truthy `Bool`
    /// - throws: One of the `JSON.Error` cases thrown by `decode(_:type:)`.
    /// - seealso: `JSON.decode(_:type:)`
    public func bool(path: JSONPathType...) throws -> Swift.Bool {
        return try Swift.Bool(json: valueAtPath(path))
    }

    /// Retrieves a `[JSON]` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - returns: An `Array` of `JSON` elements
    /// - throws: One of the `JSON.Error` cases thrown by `decode(_:type:)`.
    /// - seealso: `JSON.decode(_:type:)`
    public func array(path: JSONPathType...) throws -> [JSON] {
        return try JSON.getArray(valueAtPath(path))
    }

    /// Attempts to decodes many values from a desendant JSON array at a path
    /// into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter type: If the context this method is called from does not
    ///                   make the return type clear, pass a type implementing `JSONDecodable`
    ///                   to disambiguate the type to decode with.
    /// - returns: An `Array` of decoded elements
    /// - throws: One of the `JSON.Error` cases thrown by `decode(_:type:)`, or
    ///           any error that arises from decoding the contained values.
    /// - seealso: `JSON.decode(_:type:)`
    public func arrayOf<Decoded: JSONDecodable>(path: JSONPathType..., type: Decoded.Type = Decoded.self) throws -> [Decoded] {
        return try JSON.getArrayOf(valueAtPath(path))
    }

    /// Retrieves a `[String: JSON]` from a path into JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - returns: An `Dictionary` of `String` mapping to `JSON` elements
    /// - throws: One of the `JSON.Error` cases thrown by `decode(_:type:)`.
    /// - seealso: `JSON.decode(_:type:)`
    public func dictionary(path: JSONPathType...) throws -> [Swift.String: JSON] {
        return try JSON.getDictionary(valueAtPath(path))
    }

}

// MARK: - NotFound-Or-Null-to-Optional unpacking

extension JSON {
    
    /// An `OptionSetType` used to represent the different options available for subscripting `JSON` with `null` values or missing keys.
    /// * `.NullBecomesNil` - Treat `null` values as `nil`.
    /// * `.MissingKeyBecomesNil` - Treat missing keys as `nil`.
    public struct SubscriptingOptions: OptionSetType {
        public let rawValue: Swift.Int
        public init(rawValue: Swift.Int) {
            self.rawValue = rawValue
        }
        
        /// Treat `null` values as `nil`.
        public static let NullBecomesNil = SubscriptingOptions(rawValue: 1 << 0)
        /// Treat missing keys as `nil`.
        public static let MissingKeyBecomesNil = SubscriptingOptions(rawValue: 1 << 1)
    }
    
    private func mapOptionalAtPath<Value>(path: [JSONPathType], alongPath: SubscriptingOptions, @noescape transform: JSON throws -> Value) throws -> Value? {
        let detectNull = alongPath.contains(.NullBecomesNil)
        let detectNotFound = alongPath.contains(.MissingKeyBecomesNil)
        var json: JSON?
        do {
            json = try valueAtPath(path, detectNull: detectNull)
            return try json.map(transform)
        } catch Error.IndexOutOfBounds where detectNotFound {
            return nil
        } catch Error.KeyNotFound where detectNotFound {
            return nil
        } catch Error.ValueNotConvertible where detectNull && json == .Null {
            return nil
        } catch SubscriptError.SubscriptIntoNull where detectNull {
            return nil
        }
    }
}

extension JSON {

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
    public func decode<Decoded: JSONDecodable>(path: JSONPathType..., alongPath options: SubscriptingOptions, type: Decoded.Type = Decoded.self) throws -> Decoded? {
        return try mapOptionalAtPath(path, alongPath: options, transform: Decoded.init)
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
    public func double(path: JSONPathType..., alongPath options: SubscriptingOptions) throws -> Swift.Double? {
        return try mapOptionalAtPath(path, alongPath: options, transform: Swift.Double.init)
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
    public func int(path: JSONPathType..., alongPath options: SubscriptingOptions) throws -> Swift.Int? {
        return try mapOptionalAtPath(path, alongPath: options, transform: Swift.Int.init)
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
    public func string(path: JSONPathType..., alongPath options: SubscriptingOptions) throws -> Swift.String? {
        return try mapOptionalAtPath(path, alongPath: options, transform: Swift.String.init)
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
    public func bool(path: JSONPathType..., alongPath options: SubscriptingOptions) throws -> Swift.Bool? {
        return try mapOptionalAtPath(path, alongPath: options, transform: Swift.Bool.init)
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
    public func array(path: JSONPathType..., alongPath options: SubscriptingOptions) throws -> [JSON]? {
        return try mapOptionalAtPath(path, alongPath: options, transform: JSON.getArray)
    }

    /// Optionally decodes many values from a descendant array at a path into
    /// JSON.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter alongPath: Options that control what should be done with values that are `null` or keys that are missing.
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
    public func arrayOf<Decoded: JSONDecodable>(path: JSONPathType..., alongPath options: SubscriptingOptions, type: Decoded.Type = Decoded.self) throws -> [Decoded]? {
        return try mapOptionalAtPath(path, alongPath: options, transform: JSON.getArrayOf)
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
    public func dictionary(path: JSONPathType..., alongPath options: SubscriptingOptions) throws -> [Swift.String: JSON]? {
        return try mapOptionalAtPath(path, alongPath: options, transform: JSON.getDictionary)
    }

}

// MARK: - Missing-with-fallback unpacking

extension JSON {
    
    private func mapOptionalAtPath<Value>(path: [JSONPathType], @noescape fallback: () -> Value, @noescape transform: JSON throws -> Value) throws -> Value {
        return try mapOptionalAtPath(path, alongPath: .MissingKeyBecomesNil, transform: transform) ?? fallback()
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
    public func decode<Decoded: JSONDecodable>(path: JSONPathType..., @autoclosure or fallback: () -> Decoded) throws -> Decoded {
        return try mapOptionalAtPath(path, fallback: fallback, transform: Decoded.init)
    }
    
    /// Retrieves a `Double` from a path into JSON or a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: Array to use when one is missing at the subscript.
    /// - returns: A floating-point `Double`
    /// - throws: One of the `JSON.Error` cases thrown by calling `mapOptionalAtPath(_:fallback:transform:)`.
    /// - seealso: `optionalAtPath(_:ifNotFound)`.
    public func double(path: JSONPathType..., @autoclosure or fallback: () -> Swift.Double) throws -> Swift.Double {
        return try mapOptionalAtPath(path, fallback: fallback, transform: Swift.Double.init)
    }
    
    /// Retrieves an `Int` from a path into JSON or a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: Array to use when one is missing at the subscript.
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
    public func int(path: JSONPathType..., @autoclosure or fallback: () -> Swift.Int) throws -> Swift.Int {
        return try mapOptionalAtPath(path, fallback: fallback, transform: Swift.Int.init)
    }
    
    /// Retrieves a `String` from a path into JSON or a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: Array to use when one is missing at the subscript.
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
    public func string(path: JSONPathType..., @autoclosure or fallback: () -> Swift.String) throws -> Swift.String {
        return try mapOptionalAtPath(path, fallback: fallback, transform: Swift.String.init)
    }
    
    /// Retrieves a `Bool` from a path into JSON or a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: Array to use when one is missing at the subscript.
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
    public func bool(path: JSONPathType..., @autoclosure or fallback: () -> Swift.Bool) throws -> Swift.Bool {
        return try mapOptionalAtPath(path, fallback: fallback, transform: Swift.Bool.init)
    }
    
    /// Retrieves a `[JSON]` from a path into JSON or a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: Array to use when one is missing at the subscript.
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
    public func array(path: JSONPathType..., @autoclosure or fallback: () -> [JSON]) throws -> [JSON] {
        return try mapOptionalAtPath(path, fallback: fallback, transform: JSON.getArray)
    }
    
    /// Attempts to decodes many values from a desendant JSON array at a path
    /// into the recieving structure, returning a fallback if not found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: Array to use when one is missing at the subscript.
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
    public func arrayOf<Decoded: JSONDecodable>(path: JSONPathType..., @autoclosure or fallback: () -> [Decoded]) throws -> [Decoded] {
        return try mapOptionalAtPath(path, fallback: fallback, transform: JSON.getArrayOf)
    }
    
    /// Retrieves a `[String: JSON]` from a path into JSON or a fallback if not
    /// found.
    /// - parameter path: 0 or more `String` or `Int` that subscript the `JSON`
    /// - parameter fallback: Value to use when one is missing at the subscript
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
    public func dictionary(path: JSONPathType..., @autoclosure or fallback: () -> [Swift.String: JSON]) throws -> [Swift.String: JSON] {
        return try mapOptionalAtPath(path, fallback: fallback, transform: JSON.getDictionary)
    }
    
}

// MARK: - Deprecated methods

extension JSON {

    @available(*, deprecated, message="Use 'decode(_:alongPath:type:)' with options '[.MissingKeyBecomesNil]'")
    public func decode<Decoded: JSONDecodable>(path: JSONPathType..., ifNotFound: Swift.Bool, type: Decoded.Type = Decoded.self) throws -> Decoded? {
        let options: SubscriptingOptions = ifNotFound ? [.MissingKeyBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: Decoded.init)
    }

    @available(*, deprecated, message="Use 'decode(_:alongPath:type:)' with options '[.NullBecomesNil]'")
    public func decode<Decoded: JSONDecodable>(path: JSONPathType..., ifNull: Swift.Bool, type: Decoded.Type = Decoded.self) throws -> Decoded? {
        let options: SubscriptingOptions = ifNull ? [.NullBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: Decoded.init)
    }

    @available(*, deprecated, message="Use 'double(_:alongPath:)' with options '[.MissingKeyBecomesNil]'")
    public func double(path: JSONPathType..., ifNotFound: Swift.Bool) throws -> Swift.Double? {
        let options: SubscriptingOptions = ifNotFound ? [.MissingKeyBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: Swift.Double.init)
    }

    @available(*, deprecated, message="Use 'double(_:alongPath:)' with options '[.NullBecomesNil]'")
    public func double(path: JSONPathType..., ifNull: Swift.Bool) throws -> Swift.Double? {
        let options: SubscriptingOptions = ifNull ? [.NullBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: Swift.Double.init)
    }

    @available(*, deprecated, message="Use 'int(_:alongPath:)' with options '[.MissingKeyBecomesNil]'")
    public func int(path: JSONPathType..., ifNotFound: Swift.Bool) throws -> Swift.Int? {
        let options: SubscriptingOptions = ifNotFound ? [.MissingKeyBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: Swift.Int.init)
    }

    @available(*, deprecated, message="Use 'int(_:alongPath:)' with options '[.NullBecomesNil]'")
    public func int(path: JSONPathType..., ifNull: Swift.Bool) throws -> Swift.Int? {
        let options: SubscriptingOptions = ifNull ? [.NullBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: Swift.Int.init)
    }

    @available(*, deprecated, message="Use 'string(_:alongPath:)' with options '[.MissingKeyBecomesNil]'")
    public func string(path: JSONPathType..., ifNotFound: Swift.Bool) throws -> Swift.String? {
        let options: SubscriptingOptions = ifNotFound ? [.MissingKeyBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: Swift.String.init)
    }

    @available(*, deprecated, message="Use 'string(_:alongPath:)' with options '[.NullBecomesNil]'")
    public func string(path: JSONPathType..., ifNull: Swift.Bool) throws -> Swift.String? {
        let options: SubscriptingOptions = ifNull ? [.NullBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: Swift.String.init)
    }

    @available(*, deprecated, message="Use 'bool(_:alongPath:)' with options '[.MissingKeyBecomesNil]'")
    public func bool(path: JSONPathType..., ifNotFound: Swift.Bool) throws -> Swift.Bool? {
        let options: SubscriptingOptions = ifNotFound ? [.MissingKeyBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: Swift.Bool.init)
    }

    @available(*, deprecated, message="Use 'bool(_:alongPath:)' with options '[.NullBecomesNil]'")
    public func bool(path: JSONPathType..., ifNull: Swift.Bool) throws -> Swift.Bool? {
        let options: SubscriptingOptions = ifNull ? [.NullBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: Swift.Bool.init)
    }

    @available(*, deprecated, message="Use 'array(_:alongPath:)' with options '[.MissingKeyBecomesNil]'")
    public func array(path: JSONPathType..., ifNotFound: Swift.Bool) throws -> [JSON]? {
        let options: SubscriptingOptions = ifNotFound ? [.MissingKeyBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: JSON.getArray)
    }

    @available(*, deprecated, message="Use 'array(_:alongPath:)' with options '[.NullBecomesNil]'")
    public func array(path: JSONPathType..., ifNull: Swift.Bool) throws -> [JSON]? {
        let options: SubscriptingOptions = ifNull ? [.NullBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: JSON.getArray)
    }

    @available(*, deprecated, message="Use 'arrayOf(_:alongPath:)' with options '[.MissingKeyBecomesNil]'")
    public func arrayOf<Decoded: JSONDecodable>(path: JSONPathType..., ifNotFound: Swift.Bool) throws -> [Decoded]? {
        let options: SubscriptingOptions = ifNotFound ? [.MissingKeyBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: JSON.getArrayOf)
    }

    @available(*, deprecated, message="Use 'arrayOf(_:alongPath:)' with options '[.NullBecomesNil]'")
    public func arrayOf<Decoded: JSONDecodable>(path: JSONPathType..., ifNull: Swift.Bool) throws -> [Decoded]? {
        let options: SubscriptingOptions = ifNull ? [.NullBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: JSON.getArrayOf)
    }

    @available(*, deprecated, message="Use 'dictionary(_:alongPath:)' with options '[.MissingKeyBecomesNil]'")
    public func dictionary(path: JSONPathType..., ifNotFound: Swift.Bool) throws -> [Swift.String: JSON]? {
        let options: SubscriptingOptions = ifNotFound ? [.MissingKeyBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: JSON.getDictionary)
    }

    @available(*, deprecated, message="Use 'dictionary(_:alongPath:)' with options '[.NullBecomesNil]'")
    public func dictionary(path: JSONPathType..., ifNull: Swift.Bool) throws -> [Swift.String: JSON]? {
        let options: SubscriptingOptions = ifNull ? [.NullBecomesNil] : []
        return try mapOptionalAtPath(path, alongPath: options, transform: JSON.getDictionary)
    }

}
