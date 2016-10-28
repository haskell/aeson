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
    /// - Parameter strict: If `true`, trailing commas in arrays/objects are treated as an error. Defaults to `false`.
    /// - Returns: A `JSON` value.
    /// - Throws: `JSONParserError`
    public static func decode(_ string: String, strict: Bool = false) throws -> JSON {
        return try decode(string.unicodeScalars, strict: strict)
    }
    
    /// Decodes a sequence of `UnicodeScalar`s as JSON.
    /// - Parameter scalars: A sequence of `UnicodeScalar`s to parse as JSON.
    /// - Parameters strict: If `true`, trailing commas in arrays/objects are treated as an error. Defaults to `false`.
    /// - Returns: A `JSON` value.
    /// - Throws: `JSONParserError`
    public static func decode<Seq: Sequence>(_ scalars: Seq, strict: Bool = false) throws -> JSON where Seq.Iterator.Element == UnicodeScalar {
        var parser = JSONParser(scalars)
        parser.strict = strict
        var decoder = JSONDecoder(parser)
        return try decoder.decode()
    }
    
    /// Lazily decodes a string as a JSON stream.
    ///
    /// A JSON stream is a series of top-level JSON values. See `JSONStreamDecoder` for details.
    ///
    /// - Parameter string: A string to parse as a JSON stream.
    /// - Parameter strict: IF `true`, trailing commas in arrays/objects are treated as an error. Defaults to `false`.
    /// - Returns: A `JSONStreamDecoder`.
    public static func decodeStream(_ string: String, strict: Bool = false) -> JSONStreamDecoder<JSONParser<String.UnicodeScalarView>> {
        return decodeStream(string.unicodeScalars, strict: strict)
    }
    
    /// Lazily decodes a sequence of `UnicodeScalar`s as a JSON stream.
    ///
    /// A JSON stream is a series of top-level JSON values. See `JSONStreamDecoder` for details.
    ///
    /// - Parameter scalars: A sequence of `UnicodeScalar`s to parse as a JSON stream.
    /// - Parameter strict: If `true`, trailing commas in arrays/objects are treated as an error. Defaults to `false`.
    /// - Returns: A `JSONStreamDecoder`.
    public static func decodeStream<Seq: Sequence>(_ scalars: Seq, strict: Bool = false) -> JSONStreamDecoder<JSONParser<Seq>> where Seq.Iterator.Element == UnicodeScalar {
        var parser = JSONParser(scalars)
        parser.strict = strict
        parser.streaming = true
        return JSONStreamDecoder(parser)
    }
}

/// A JSON decoder that consumes a stream of JSON events.
///
/// In most cases, you should use the convenience method `JSON.decode(_:)` to decode values
/// instead of using this class directly.
///
/// - SeeAlso: `JSONParser`.
public struct JSONDecoder<Seq: Sequence> where Seq.Iterator: JSONEventIterator, Seq.Iterator.Element == JSONEvent {
    public init(_ parser: Seq) {
        iter = parser.makeIterator()
    }
    
    /// If `true`, the decoder will operate in streaming mode, allowing for multiple
    /// top-level json values, with each call to `decode()` returning a successive value.
    /// The default value of `false` means that `decode()` can only be called once, and
    /// any JSON events past the first top-level value are considered an error.
    ///
    /// See `decode()` for more details on the streaming operation.
    ///
    /// - Important: When wrapping a `JSONParser`, the parser must separately be put into
    ///   streaming mode, as `JSONDecoder` operates generically over a sequence of `JSONEvent`s.
    public var streaming: Bool = false
    
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
        if streaming && token == nil {
            throw JSONDecoderError.streamEnded
        }
        let result = try buildValue()
        if !streaming {
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
        streaming = true
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
    
    private mutating func buildValue() throws -> JSON {
        switch token {
        case .objectStart?: return try buildObject()
        case .objectEnd?: throw error(.invalidSyntax)
        case .arrayStart?: return try buildArray()
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
    
    private mutating func buildObject() throws -> JSON {
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
            dict[key] = try buildValue()
            bump()
        }
        throw error(.unexpectedEOF)
    }
    
    private mutating func buildArray() throws -> JSON {
        bump()
        var ary: JSONArray = []
        while let token = self.token {
            if case .arrayEnd = token {
                return .array(ary)
            }
            ary.append(try buildValue())
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

/// Errors that may be thrown by the `JSONDecoder` during the decode stage.
public enum JSONDecoderError: Error {
    /// Signals that a `JSONDecoder` operating in streaming mode has reached the end of the stream.
    case streamEnded
    /// Thrown when a `JSONDecoder` operating in one-shot mode finds extra tokens after the first top-level JSON value.
    case unexpectedToken
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
    public init(_ parser: Seq) {
        decoder = JSONDecoder(parser)
        decoder.streaming = true
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
