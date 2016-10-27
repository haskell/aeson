//
//  Decoder.swift
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

extension JSON {
    /// Decodes a string as JSON.
    /// - Parameter string: A string to parse as JSON.
    /// - Parameter options: Options that controls JSON parsing. Defaults to no options. See `JSONOptions` for details.
    /// - Returns: A `JSON` value.
    /// - Throws: `JSONParserError`
    public static func decode(_ string: String, options: JSONOptions = []) throws -> JSON {
        return try decode(string.unicodeScalars, options: options)
    }
    
    @available(*, deprecated, message: "Use JSON.decode(_:options:) instead")
    public static func decode(_ string: String, strict: Bool) throws -> JSON {
        return try decode(string, options: JSONOptions(strict: strict))
    }
    
    /// Decodes a sequence of `UnicodeScalar`s as JSON.
    /// - Parameter scalars: A sequence of `UnicodeScalar`s to parse as JSON.
    /// - Parameter options: Options that controls JSON parsing. Defaults to no options. See `JSONOptions` for details.
    /// - Returns: A `JSON` value.
    /// - Throws: `JSONParserError`
    public static func decode<Seq: Sequence>(_ scalars: Seq, options: JSONOptions = []) throws -> JSON where Seq.Iterator.Element == UnicodeScalar {
        let parser = JSONParser(scalars, options: options.parserOptions)
        var decoder = JSONDecoder(parser, options: options.decoderOptions)
        return try decoder.decode()
    }
    
    @available(*, deprecated, message: "Use JSON.decode(_:options:) instead")
    public static func decode<Seq: Sequence>(_ scalars: Seq, strict: Bool) throws -> JSON where Seq.Iterator.Element == UnicodeScalar {
        return try decode(scalars, options: JSONOptions(strict: strict))
    }
    
    /// Lazily decodes a string as a JSON stream.
    ///
    /// A JSON stream is a series of top-level JSON values. See `JSONStreamDecoder` for details.
    ///
    /// - Parameter string: A string to parse as a JSON stream.
    /// - Parameter options: Options that controls JSON parsing. Defaults to no options. See `JSONOptions` for details.
    /// - Returns: A `JSONStreamDecoder`.
    public static func decodeStream(_ string: String, options: JSONOptions = []) -> JSONStreamDecoder<JSONParser<String.UnicodeScalarView>> {
        return decodeStream(string.unicodeScalars, options: options)
    }
    
    @available(*, deprecated, message: "Use JSON.decodeStream(_:options:) instead")
    public static func decodeStream(_ string: String, strict: Bool) -> JSONStreamDecoder<JSONParser<String.UnicodeScalarView>> {
        return decodeStream(string, options: JSONOptions(strict: strict))
    }
    
    /// Lazily decodes a sequence of `UnicodeScalar`s as a JSON stream.
    ///
    /// A JSON stream is a series of top-level JSON values. See `JSONStreamDecoder` for details.
    ///
    /// - Parameter scalars: A sequence of `UnicodeScalar`s to parse as a JSON stream.
    /// - Parameter options: Options that controls JSON parsing. Defaults to no options. See `JSONOptions` for details.
    /// - Returns: A `JSONStreamDecoder`.
    public static func decodeStream<Seq: Sequence>(_ scalars: Seq, options: JSONOptions = []) -> JSONStreamDecoder<JSONParser<Seq>> where Seq.Iterator.Element == UnicodeScalar {
        var parserOptions = options.parserOptions
        parserOptions.streaming = true
        let parser = JSONParser(scalars, options: parserOptions)
        return JSONStreamDecoder(parser)
    }
    
    @available(*, deprecated, message: "Use JSON.decodeStream(_:options:) instead")
    public static func decodeStream<Seq: Sequence>(_ scalars: Seq, strict: Bool) -> JSONStreamDecoder<JSONParser<Seq>> where Seq.Iterator.Element == UnicodeScalar {
        return decodeStream(scalars, options: JSONOptions(strict: strict))
    }
}

/// Options that can be used with `JSON.decode(…)`.
public struct JSONOptions {
    /// If `true`, the parser strictly conforms to RFC 7159.
    /// If `false`, the parser accepts the following extensions:
    /// - Trailing commas.
    /// - Less restrictive about numbers, such as `-01` or `-.123`.
    ///
    /// The default value is `false`.
    public var strict: Bool = false
    
    /// A maximum depth limit to apply to nested arrays/dictionaries.
    /// If `nil`, there is no depth limit.
    ///
    /// The default value is `10_000`.
    public var depthLimit: Int? = 10_000
    
    /// Returns a new `JSONOptions` with default values.
    public init() {}
    
    /// Returns a new `JSONOptions`.
    /// - Parameter strict: Whether the parser should be strict. Defaults to `false`.
    /// - Parameter depthLimit: A maximum depth limit to use. Default is `10_000`.
    public init(strict: Bool = false, depthLimit: Int? = 10_000) {
        self.strict = strict
        self.depthLimit = depthLimit
    }
    
    public var decoderOptions: JSONDecoderOptions {
        return JSONDecoderOptions(depthLimit: depthLimit)
    }
    
    public var streamDecoderOptions: JSONStreamDecoderOptions {
        return JSONStreamDecoderOptions(depthLimit: depthLimit)
    }
    
    public var parserOptions: JSONParserOptions {
        return JSONParserOptions(strict: strict)
    }
}

extension JSONOptions: ExpressibleByArrayLiteral {
    public enum Element {
        /// Makes the parser strictly conform to RFC 7159.
        /// - SeeAlso: `JSONOptions.strict`.
        case strict
        /// Sets a maximum depth limit for nested arrays/dictionaries.
        /// If specified multiple times, the last specified limit is used.
        /// Specifying `nil` removes the default depth limit of `10_000`.
        /// - SeeAlso: `JSONOptions.depthLimit`.
        case depthLimit(Int?)
    }
    
    public init(arrayLiteral elements: Element...) {
        for elt in elements {
            switch elt {
            case .strict: strict = true
            case .depthLimit(let limit):
                depthLimit = limit
            }
        }
    }
}

/// A JSON decoder that consumes a stream of JSON events.
///
/// In most cases, you should use the convenience method `JSON.decode(_:)` to decode values
/// instead of using this class directly.
///
/// - SeeAlso: `JSONParser`.
public struct JSONDecoder<Seq: Sequence> where Seq.Iterator: JSONEventIterator, Seq.Iterator.Element == JSONEvent {
    public init(_ parser: Seq, options: JSONDecoderOptions = []) {
        iter = parser.makeIterator()
        self.options = options
    }
    
    /// Options to apply to the decoder.
    /// See `JSONDecoderOptions` for details.
    var options: JSONDecoderOptions
    
    @available(*, deprecated, renamed: "options.streaming")
    public var streaming: Bool {
        get { return options.streaming }
        set { options.streaming = newValue }
    }
    
    /// Decodes and returns a top-level JSON value from the event stream.
    ///
    /// When `streaming` is `false`, any events after the top-level value has been consumed
    /// will throw an error, and any subsequent calls to `decode()` after the first call
    /// will also throw an error. When `streaming` is `false`, after a top-level value has
    /// been decoded, no more events will be consumed, and `decode()` can be called repeatedly
    /// to decode more top-level events.
    ///
    /// Because the normal operation of this class is to decode one-shot values rather than
    /// streams, this method returns a non-optional value. As such, when operating in a
    /// streaming manner, this method will throw the special error `JSONDecoderError.streamEnded`
    /// to signal that there are no more values. To make this easier to work with, a convenience
    /// method `decodeStream()` is provided that returns an array of `JSON` values and automatically
    /// handles `JSONDecoderError.streamEnded`, and the type `JSONStreamDecoder` provides a
    /// lazy sequence interface to decoding a JSON stream.
    ///
    /// - Returns: A single top-level `JSON` value.
    ///
    /// - Throws: `JSONParserError`, `JSONDecoderError`.
    public mutating func decode() throws -> JSON {
        bump()
        if options.streaming && token == nil {
            throw JSONDecoderError.streamEnded
        }
        let result = try buildValue(depth: 0)
        if !options.streaming {
            bump()
            switch token {
            case .none: break
            case .some(.error(let err)): throw err
            case .some: throw JSONDecoderError.unexpectedToken
            }
        }
        return result
    }
    
    /// Decodes and returns an array of top-level JSON values from the event stream.
    ///
    /// This is a convenience method that sets `streaming` to `true` and then decodes
    /// as many top-level JSON values as it can before the stream ends.
    ///
    /// - Returns: An array of top-level `JSON` values.
    ///
    /// - SeeAlso: `JSONStreamDecoder`.
    public mutating func decodeStream() throws -> [JSON] {
        options.streaming = true
        var results: [JSON] = []
        repeat {
            do {
                results.append(try decode())
            } catch JSONDecoderError.streamEnded {
                return results
            }
        } while true
    }
    
    private mutating func bump() {
        token = iter.next()
    }
    
    private mutating func buildValue(depth: Int) throws -> JSON {
        switch token {
        case .objectStart?:
            let newDepth = depth + 1
            if let limit = options.depthLimit, newDepth > limit {
                throw JSONDecoderError.exceededDepthLimit
            }
            return try buildObject(depth: newDepth)
        case .objectEnd?: throw error(.invalidSyntax)
        case .arrayStart?:
            let newDepth = depth + 1
            if let limit = options.depthLimit, newDepth > limit {
                throw JSONDecoderError.exceededDepthLimit
            }
            return try buildArray(depth: newDepth)
        case .arrayEnd?: throw error(.invalidSyntax)
        case .booleanValue(let b)?: return .bool(b)
        case .int64Value(let i)?: return .int64(i)
        case .doubleValue(let d)?: return .double(d)
        case .stringValue(let s)?: return .string(s)
        case .nullValue?: return .null
        case .error(let err)?: throw err
        case nil: throw error(.unexpectedEOF)
        }
    }
    
    private mutating func buildObject(depth: Int) throws -> JSON {
        bump()
        var dict: [String: JSON] = Dictionary(minimumCapacity: objectHighWaterMark)
        defer { objectHighWaterMark = max(objectHighWaterMark, dict.count) }
        while let token = self.token {
            let key: String
            switch token {
            case .objectEnd: return .object(JSONObject(dict))
            case .error(let err): throw err
            case .stringValue(let s): key = s
            default: throw error(.nonStringKey)
            }
            bump()
            dict[key] = try buildValue(depth: depth)
            bump()
        }
        throw error(.unexpectedEOF)
    }
    
    private mutating func buildArray(depth: Int) throws -> JSON {
        bump()
        var ary: JSONArray = []
        while let token = self.token {
            if case .arrayEnd = token {
                return .array(ary)
            }
            ary.append(try buildValue(depth: depth))
            bump()
        }
        throw error(.unexpectedEOF)
    }
    
    private func error(_ code: JSONParserError.Code) -> JSONParserError {
        return JSONParserError(code: code, line: iter.line, column: iter.column)
    }
    
    private var iter: Seq.Iterator
    private var token: JSONEvent?
    private var objectHighWaterMark: Int = 0
}

/// Options that can be used with `JSONDecoder`.
public struct JSONDecoderOptions {
    /// A maximum depth limit to apply to nested arrays/dictionaries.
    /// If `nil`, there is no depth limit.
    ///
    /// The default value is `10_000`.
    public var depthLimit: Int? = 10_000
    
    /// If `true`, the decoder will operate in streaming mode, allowing for multiple
    /// top-level json values, with each call to `decode()` returning a successive value.
    /// The default value of `false` means that `decode()` can only be called once, and
    /// any JSON events past the first top-level value are considered an error.
    ///
    /// See `JSONDecoder.decode()` for more details on the streaming operation.
    ///
    /// The default value is `false`.
    ///
    /// - Important: When wrapping a `JSONParser`, the parser must separately be put into
    ///   streaming mode, as `JSONDecoder` operates generically over a sequence of `JSONEvent`s.
    public var streaming: Bool = false
    
    /// Returns a new `JSONDecoderOptions` with default values.
    public init() {}
    
    /// Returns a new `JSONDecoderOptions`.
    /// - Parameter depthLimit: A maximum depth limit to use. Default is `10_000`.
    /// - Parameter streaming: Whether the decode should operate in streaming mode. Default is `false`.
    public init(depthLimit: Int? = 10_000, streaming: Bool = false) {
        self.depthLimit = depthLimit
        self.streaming = streaming
    }
}

extension JSONDecoderOptions: ExpressibleByArrayLiteral {
    public enum Element {
        /// Sets a maximum depth limit for nested arrays/dictionaries.
        /// If specified multiple times, the last specified limit is used.
        /// Specifying `nil` removes the default depth limit of `10_000`.
        /// - SeeAlso: `JSONDecoderOptions.depthLimit`.
        case depthLimit(Int?)
        /// Puts the decoder into streaming mode.
        /// - SeeAlso: `JSONDecoderOptions.streaming`.
        case streaming
    }
    
    public init(arrayLiteral elements: Element...) {
        for elt in elements {
            switch elt {
            case .depthLimit(let limit):
                depthLimit = limit
            case .streaming:
                streaming = true
            }
        }
    }
}

/// Errors that may be thrown by the `JSONDecoder` during the decode stage.
public enum JSONDecoderError: Error {
    /// Signals that a `JSONDecoder` operating in streaming mode has reached the end of the stream.
    case streamEnded
    /// Thrown when a `JSONDecoder` operating in one-shot mode finds extra tokens after the first top-level JSON value.
    case unexpectedToken
    /// Thrown when a `JSONDecoder` exceeds the specified depth limit.
    case exceededDepthLimit
}

/// A JSON decoder that decodes a stream of JSON events into a lazy sequence of top-level JSON values.
///
/// This is a sequence of zero or more `JSONStreamValue.json` values, ending with zero or one `JSONStreamValue.error` value.
///
/// - Important: When wrapping a `JSONParser`, the parser must separately be put into streaming mode, as `JSONStreamDecoder`
///   operates generically over a sequence of `JSONEvent`s. You should consider using `JSON.decodeStream(_:)` instead to
///   create the stream decoder.
///
/// - SeeAlso: `JSON.decodeStream(_:)`.
public struct JSONStreamDecoder<Seq: Sequence>: Sequence, IteratorProtocol where Seq.Iterator: JSONEventIterator, Seq.Iterator.Element == JSONEvent {
    public init(_ parser: Seq, options: JSONStreamDecoderOptions = []) {
        decoder = JSONDecoder(parser, options: options.decoderOptions)
    }
    
    /// Options to apply to the decoder.
    /// See `JSONStreamDecoderOptions` for details.
    public var options: JSONStreamDecoderOptions {
        get { return decoder.options.streamDecoderOptions }
        set { decoder.options.update(with: newValue) }
    }
    
    /// Returns an array of all decoded values, or throws an error if one occurs.
    ///
    /// This eagerly decodes the rest of the JSON stream and returns all values. If a parse error occurs at any point,
    /// the error is thrown and all values are discarded.
    ///
    /// - Returns: An array of `JSON` values.
    /// - Throws: `JSONParserError`.
    public func values() throws -> [JSON] {
        return try map({ try $0.unwrap() })
    }
    
    public func makeIterator() -> JSONStreamDecoder<Seq> {
        return self
    }
    
    public mutating func next() -> JSONStreamValue? {
        do {
            return try JSONStreamValue.json(decoder.decode())
        } catch JSONDecoderError.streamEnded {
            return nil
        } catch let error as JSONParserError {
            return JSONStreamValue.error(error)
        } catch {
            // This shouldn't be reachable. A `JSONDecoder` operating in streaming mode can throw only
            // `JSONParserError`s or `JSONDecoderError.streamEnded`.
            return nil
        }
    }
    
    private var decoder: JSONDecoder<Seq>
}

/// Options that can be used with `JSONStreamDecoder`.
public struct JSONStreamDecoderOptions {
    /// A maximum depth limit to apply to nested arrays/dictionaries.
    /// If `nil`, there is no depth limit.
    ///
    /// The default value is `10_000`.
    public var depthLimit: Int? = 10_000
    
    /// Returns a new `JSONStreamDecoderOptions` with default values.
    public init() {}
    
    /// Returns a new `JSONStreamDecoderOptions`.
    /// - Parameter depthLimit: A maximum depth limit to use. Default is `10_000`.
    public init(depthLimit: Int? = 10_000) {
        self.depthLimit = depthLimit
    }
    
    fileprivate var decoderOptions: JSONDecoderOptions {
        return JSONDecoderOptions(depthLimit: depthLimit, streaming: true)
    }
}

private extension JSONDecoderOptions {
    var streamDecoderOptions: JSONStreamDecoderOptions {
        return JSONStreamDecoderOptions(depthLimit: depthLimit)
    }
    
    mutating func update(with options: JSONStreamDecoderOptions) {
        depthLimit = options.depthLimit
    }
}

extension JSONStreamDecoderOptions: ExpressibleByArrayLiteral {
    public enum Element {
        /// Sets a maximum depth limit for nested arrays/dictionaries.
        /// If specified multiple times, the last specified limit is used.
        /// Specifying `nil` removes the default depth limit of `10_000`.
        /// - SeeAlso: `JSONStreamDecoderOptions.depthLimit`.
        case depthLimit(Int?)
    }
    
    public init(arrayLiteral elements: Element...) {
        for elt in elements {
            switch elt {
            case .depthLimit(let limit):
                depthLimit = limit
            }
        }
    }
}

public enum JSONStreamValue: Equatable {
    case json(JSON)
    case error(JSONParserError)
    
    /// Returns the contained `JSON` value, otherwise `nil`.
    public var json: JSON? {
        switch self {
        case .json(let json): return json
        case .error: return nil
        }
    }
    
    /// Returns the contained error, otherwise `nil`.
    public var error: JSONParserError? {
        switch self {
        case .json: return nil
        case .error(let error): return error
        }
    }
    
    /// Unwraps the contained `JSON` value or throws the contained error.
    ///
    /// - Throws: `JSONParserError`.
    public func unwrap() throws -> JSON {
        switch self {
        case .json(let value): return value
        case .error(let error): throw error
        }
    }
    
    public static func ==(lhs: JSONStreamValue, rhs: JSONStreamValue) -> Bool {
        switch (lhs, rhs) {
        case let (.json(a), .json(b)): return a == b
        case let (.error(a), .error(b)): return a == b
        default: return false
        }
    }
}
