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

#if os(Linux)
    import Glibc
#else
    import Darwin
#endif

/// A streaming JSON parser that consumes a sequence of unicode scalars.
public struct JSONParser<Seq: Sequence>: Sequence where Seq.Iterator.Element == UnicodeScalar {
    public init(_ seq: Seq) {
        base = seq
    }
    
    /// If `true`, trailing commas in dictionaries and arrays are treated as an error.
    /// Defaults to `false`.
    public var strict: Bool = false
    
    /// If `true`, the parser will parse a stream of json values with optional whitespace delimiters.
    /// The default value of `false` makes the parser emit an error if there's any non-whitespace
    /// characters after the first JSON value.
    ///
    /// For example, with the input `"[1] [2,3]"`, if `streaming` is `true` the parser will emit
    /// events for the second JSON array after the first one, but if `streaming` is `false` it will
    /// emit an error upon encountering the second `[`.
    ///
    /// - Note: If `streaming` is `true` and the input is empty (or contains only whitespace), the
    ///   parser will return `nil` instead of emitting an `.unexpectedEOF` error.
    public var streaming: Bool = false
    
    public func makeIterator() -> JSONParserIterator<Seq.Iterator> {
        var iter = JSONParserIterator(base.makeIterator())
        iter.strict = strict
        iter.streaming = streaming
        return iter
    }
    
    private let base: Seq
}

/// The iterator for `JSONParser`.
public struct JSONParserIterator<Iter: IteratorProtocol>: JSONEventIterator where Iter.Element == UnicodeScalar {
    public init(_ iter: Iter) {
        base = PeekIterator(iter)
    }
    
    /// If `true`, trailing commas in dictionaries and arrays are treated as an error.
    /// Defaults to `false`.
    public var strict: Bool = false
    
    /// If `true`, the parser will parse a stream of json values with optional whitespace delimiters.
    /// The default value of `false` makes the parser emit an error if there's any non-whitespace
    /// characters after the first JSON value.
    ///
    /// For example, with the input `"[1] [2,3]"`, if `streaming` is `true` the parser will emit
    /// events for the second JSON array after the first one, but if `streaming` is `false` it will
    /// emit an error upon encountering the second `[`.
    ///
    /// - Note: If `streaming` is `true` and the input is empty (or contains only whitespace), the
    ///   parser will return `nil` instead of emitting an `.unexpectedEOF` error.
    public var streaming: Bool = false
    
    public mutating func next() -> JSONEvent? {
        do {
            // the only states that may loop are parseArrayComma, parseObjectComma, and (if streaming) parseEnd,
            // which are all guaranteed to shift to other states (if they don't return) so the loop is finite
            while true {
                switch state {
                case .parseArrayComma:
                    switch skipWhitespace() {
                    case ","?:
                        state = .parseArray(first: false)
                        continue
                    case "]"?:
                        try popStack()
                        return .arrayEnd
                    case .some:
                        throw error(.invalidSyntax)
                    case nil:
                        throw error(.unexpectedEOF)
                    }
                case .parseObjectComma:
                    switch skipWhitespace() {
                    case ","?:
                        state = .parseObjectKey(first: false)
                        continue
                    case "}"?:
                        try popStack()
                        return .objectEnd
                    case .some:
                        throw error(.invalidSyntax)
                    case nil:
                        throw error(.unexpectedEOF)
                    }
                case .initial:
                    guard let c = skipWhitespace() else {
                        if streaming {
                            state = .finished
                            return nil
                        } else {
                            throw error(.unexpectedEOF)
                        }
                    }
                    let evt = try parseValue(c)
                    switch evt {
                    case .arrayStart, .objectStart:
                        break
                    default:
                        state = .parseEnd
                    }
                    return evt
                case .parseArray(let first):
                    guard let c = skipWhitespace() else { throw error(.unexpectedEOF) }
                    switch c {
                    case "]":
                        if !first && strict {
                            throw error(.trailingComma)
                        }
                        try popStack()
                        return .arrayEnd
                    case ",":
                        throw error(.missingValue)
                    default:
                        let evt = try parseValue(c)
                        switch evt {
                        case .arrayStart, .objectStart:
                            break
                        default:
                            state = .parseArrayComma
                        }
                        return evt
                    }
                case .parseObjectKey(let first):
                    guard let c = skipWhitespace() else { throw error(.unexpectedEOF) }
                    switch c {
                    case "}":
                        if !first && strict {
                            throw error(.trailingComma)
                        }
                        try popStack()
                        return .objectEnd
                    case ",", ":":
                        throw error(.missingKey)
                    default:
                        let evt = try parseValue(c)
                        switch evt {
                        case .stringValue:
                            state = .parseObjectValue
                        default:
                            throw error(.nonStringKey)
                        }
                        return evt
                    }
                case .parseObjectValue:
                    guard skipWhitespace() == ":" else { throw error(.expectedColon) }
                    guard let c = skipWhitespace() else { throw error(.unexpectedEOF) }
                    switch c {
                    case ",", "}":
                        throw error(.missingValue)
                    default:
                        let evt = try parseValue(c)
                        switch evt {
                        case .arrayStart, .objectStart:
                            break
                        default:
                            state = .parseObjectComma
                        }
                        return evt
                    }
                case .parseEnd:
                    if streaming {
                        state = .initial
                    } else if skipWhitespace() != nil {
                        throw error(.trailingCharacters)
                    } else {
                        state = .finished
                        return nil
                    }
                case .finished:
                    return nil
                }
            }
        } catch let error as JSONParserError {
            state = .finished
            return .error(error)
        } catch {
            fatalError("unexpected error \(error)")
        }
    }
    
    private mutating func popStack() throws {
        if stack.popLast() == nil {
            fatalError("exhausted stack")
        }
        switch stack.last {
        case .array?:
            state = .parseArrayComma
        case .object?:
            state = .parseObjectComma
        case nil:
            state = .parseEnd
        }
    }
    
    private mutating func parseValue(_ c: UnicodeScalar) throws -> JSONEvent {
        switch c {
        case "[":
            state = .parseArray(first: true)
            stack.append(.array)
            return .arrayStart
        case "{":
            state = .parseObjectKey(first: true)
            stack.append(.object)
            return .objectStart
        case "\"":
            var scalars = String.UnicodeScalarView()
            while let c = bump() {
                switch c {
                case "\"":
                    return .stringValue(String(scalars))
                case "\\":
                    let c = try bumpRequired()
                    switch c {
                    case "\"", "\\", "/": scalars.append(c)
                    case "b": scalars.append(UnicodeScalar(0x8))
                    case "f": scalars.append(UnicodeScalar(0xC))
                    case "n": scalars.append("\n" as UnicodeScalar)
                    case "r": scalars.append("\r" as UnicodeScalar)
                    case "t": scalars.append("\t" as UnicodeScalar)
                    case "u":
                        let codeUnit = try parseFourHex()
                        if UTF16.isLeadSurrogate(codeUnit) {
                            guard try (bumpRequired() == "\\" && bumpRequired() == "u") else {
                                throw error(.loneLeadingSurrogateInUnicodeEscape)
                            }
                            let trail = try parseFourHex()
                            if UTF16.isTrailSurrogate(trail) {
                                let lead = UInt32(codeUnit)
                                let trail = UInt32(trail)
                                // NB: The following is split up to avoid exponential time complexity in the type checker
                                let leadComponent: UInt32 = (lead - 0xD800) << 10
                                let trailComponent: UInt32 = trail - 0xDC00
                                let scalar = UnicodeScalar(leadComponent + trailComponent + 0x10000)!
                                scalars.append(scalar)
                            } else {
                                throw error(.loneLeadingSurrogateInUnicodeEscape)
                            }
                        } else {
                            scalars.append(UnicodeScalar(codeUnit)!)
                        }
                    default:
                        throw error(.invalidEscape)
                    }
                case "\0"..."\u{1F}":
                    throw error(.invalidSyntax)
                default:
                    scalars.append(c)
                }
            }
            throw error(.unexpectedEOF)
        case "-", "0"..."9":
            var tempBuffer: ContiguousArray<Int8>
            if let buffer = replace(&self.tempBuffer, with: nil) {
                tempBuffer = buffer
                tempBuffer.removeAll(keepingCapacity: true)
            } else {
                tempBuffer = ContiguousArray()
                tempBuffer.reserveCapacity(12)
            }
            defer { self.tempBuffer = tempBuffer }
            tempBuffer.append(Int8(truncatingBitPattern: c.value))
            outerLoop: while let c = base.peek() {
                switch c {
                case "0"..."9":
                    bump()
                    tempBuffer.append(Int8(truncatingBitPattern: c.value))
                case ".":
                    bump()
                    tempBuffer.append(Int8(truncatingBitPattern: c.value))
                    guard let c = bump(), case "0"..."9" = c else { throw error(.invalidNumber) }
                    tempBuffer.append(Int8(truncatingBitPattern: c.value))
                    loop: while let c = base.peek() {
                        switch c {
                        case "0"..."9":
                            bump()
                            tempBuffer.append(Int8(truncatingBitPattern: c.value))
                        case "e", "E":
                            bump()
                            tempBuffer.append(Int8(truncatingBitPattern: c.value))
                            guard let c = bump() else { throw error(.invalidNumber) }
                            tempBuffer.append(Int8(truncatingBitPattern: c.value))
                            switch c {
                            case "-", "+":
                                guard let c = bump(), case "0"..."9" = c else { throw error(.invalidNumber) }
                                tempBuffer.append(Int8(truncatingBitPattern: c.value))
                            case "0"..."9": break
                            default: throw error(.invalidNumber)
                            }
                            while let c = base.peek() {
                                switch c {
                                case "0"..."9":
                                    bump()
                                    tempBuffer.append(Int8(truncatingBitPattern: c.value))
                                default:
                                    break loop
                                }
                            }
                            break loop
                        default:
                            break loop
                        }
                    }
                    tempBuffer.append(0)
                    return .doubleValue(tempBuffer.withUnsafeBufferPointer({strtod($0.baseAddress, nil)}))
                case "e", "E":
                    bump()
                    tempBuffer.append(Int8(truncatingBitPattern: c.value))
                    guard let c = bump(), case "0"..."9" = c else { throw error(.invalidNumber) }
                    tempBuffer.append(Int8(truncatingBitPattern: c.value))
                    loop: while let c = base.peek() {
                        switch c {
                        case "0"..."9":
                            bump()
                            tempBuffer.append(Int8(truncatingBitPattern: c.value))
                        default:
                            break loop
                        }
                    }
                    tempBuffer.append(0)
                    return .doubleValue(tempBuffer.withUnsafeBufferPointer({strtod($0.baseAddress, nil)}))
                default:
                    break outerLoop
                }
            }
            if tempBuffer.count == 1 && tempBuffer[0] == 0x2d /* - */ {
                throw error(.invalidNumber)
            }
            tempBuffer.append(0)
            let num = tempBuffer.withUnsafeBufferPointer({ ptr -> Int64? in
                errno = 0
                let n = strtoll(ptr.baseAddress, nil, 10)
                if n == 0 && errno != 0 {
                    return nil
                } else {
                    return n
                }
            })
            if let num = num {
                return .int64Value(num)
            }
            // out of range, fall back to Double
            return .doubleValue(tempBuffer.withUnsafeBufferPointer({strtod($0.baseAddress, nil)}))
        case "t":
            let line = self.line, column = self.column
            guard case "r"? = bump(), case "u"? = bump(), case "e"? = bump() else {
                throw JSONParserError(code: .invalidSyntax, line: line, column: column)
            }
            return .booleanValue(true)
        case "f":
            let line = self.line, column = self.column
            guard case "a"? = bump(), case "l"? = bump(), case "s"? = bump(), case "e"? = bump() else {
                throw JSONParserError(code: .invalidSyntax, line: line, column: column)
            }
            return .booleanValue(false)
        case "n":
            let line = self.line, column = self.column
            guard case "u"? = bump(), case "l"? = bump(), case "l"? = bump() else {
                throw JSONParserError(code: .invalidSyntax, line: line, column: column)
            }
            return .nullValue
        default:
            throw error(.invalidSyntax)
        }
    }
    
    private mutating func skipWhitespace() -> UnicodeScalar? {
        while let c = bump() {
            switch c {
            case " ", "\t", "\n", "\r": continue
            default: return c
            }
        }
        return nil
    }
    
    private mutating func parseFourHex() throws -> UInt16 {
        var codepoint: UInt32 = 0
        for _ in 0..<4 {
            let c = try bumpRequired()
            codepoint <<= 4
            switch c {
            case "0"..."9":
                codepoint += c.value - 48
            case "a"..."f":
                codepoint += c.value - 87
            case "A"..."F":
                codepoint += c.value - 55
            default:
                throw error(.invalidEscape)
            }
        }
        return UInt16(truncatingBitPattern: codepoint)
    }
    
    @inline(__always) @discardableResult private mutating func bump() -> UnicodeScalar? {
        let c = base.next()
        if c == "\n" {
            line += 1
            column = 0
        } else {
            column += 1
        }
        return c
    }
    
    @inline(__always) private mutating func bumpRequired() throws -> UnicodeScalar {
        guard let c = bump() else { throw error(.unexpectedEOF) }
        return c
    }
    
    private func error(_ code: JSONParserError.Code) -> JSONParserError {
        return JSONParserError(code: code, line: line, column: column)
    }
    
    /// The line of the last emitted token.
    public private(set) var line: UInt = 0
    /// The column of the last emitted token.
    public private(set) var column: UInt = 0
    
    private var base: PeekIterator<Iter>
    private var state: State = .initial
    private var stack: [Stack] = []
    private var tempBuffer: ContiguousArray<Int8>?
}

@available(*, renamed: "JSONParserIterator")
typealias JSONParserGenerator<Gen: IteratorProtocol> = JSONParserIterator<Gen> where Gen.Element == UnicodeScalar

private enum State {
    /// Initial state
    case initial
    /// Parse an element or the end of the array
    case parseArray(first: Bool)
    /// Parse a comma or the end of the array
    case parseArrayComma
    /// Parse an object key or the end of the array
    case parseObjectKey(first: Bool)
    /// Parse a colon followed by an object value
    case parseObjectValue
    /// Parse a comma or the end of the object
    case parseObjectComma
    /// Parse whitespace or EOF
    case parseEnd
    /// Parsing has completed
    case finished
}

private enum Stack {
    case array
    case object
}

/// A streaming JSON parser event.
public enum JSONEvent: Hashable {
    /// The start of an object.
    /// Inside of an object, each key/value pair is emitted as a
    /// `StringValue` for the key followed by the `JSONEvent` sequence
    /// that describes the value.
    case objectStart
    /// The end of an object.
    case objectEnd
    /// The start of an array.
    case arrayStart
    /// The end of an array.
    case arrayEnd
    /// A boolean value.
    case booleanValue(Bool)
    /// A signed 64-bit integral value.
    case int64Value(Int64)
    /// A double value.
    case doubleValue(Double)
    /// A string value.
    case stringValue(String)
    /// The null value.
    case nullValue
    /// A parser error.
    case error(JSONParserError)
    
    public var hashValue: Int {
        switch self {
        case .objectStart: return 1
        case .objectEnd: return 2
        case .arrayStart: return 3
        case .arrayEnd: return 4
        case .booleanValue(let b): return b.hashValue << 4 + 5
        case .int64Value(let i): return i.hashValue << 4 + 6
        case .doubleValue(let d): return d.hashValue << 4 + 7
        case .stringValue(let s): return s.hashValue << 4 + 8
        case .nullValue: return 9
        case .error(let error): return error.hashValue << 4 + 10
        }
    }
    
    public static func ==(lhs: JSONEvent, rhs: JSONEvent) -> Bool {
        switch (lhs, rhs) {
        case (.objectStart, .objectStart), (.objectEnd, .objectEnd),
             (.arrayStart, .arrayStart), (.arrayEnd, .arrayEnd), (.nullValue, .nullValue):
            return true
        case let (.booleanValue(a), .booleanValue(b)):
            return a == b
        case let (.int64Value(a), .int64Value(b)):
            return a == b
        case let (.doubleValue(a), .doubleValue(b)):
            return a == b
        case let (.stringValue(a), .stringValue(b)):
            return a == b
        case let (.error(a), .error(b)):
            return a == b
        default:
            return false
        }
    }
}

/// An iterator of `JSONEvent`s that records column/line info.
public protocol JSONEventIterator: IteratorProtocol {
    /// The line of the last emitted token.
    var line: UInt { get }
    /// The column of the last emitted token.
    var column: UInt { get }
}

@available(*, renamed: "JSONEventIterator")
public typealias JSONEventGenerator = JSONEventIterator

public struct JSONParserError: Error, Hashable, CustomStringConvertible {
    /// A generic syntax error.
    public static let invalidSyntax: Code = .invalidSyntax
    /// An invalid number.
    public static let invalidNumber: Code = .invalidNumber
    /// An invalid string escape.
    public static let invalidEscape: Code = .invalidEscape
    /// A unicode string escape with an invalid code point.
    public static let invalidUnicodeScalar: Code = .invalidUnicodeScalar
    /// A unicode string escape representing a leading surrogate without
    /// a corresponding trailing surrogate.
    public static let loneLeadingSurrogateInUnicodeEscape: Code = .loneLeadingSurrogateInUnicodeEscape
    /// A control character in a string.
    public static let controlCharacterInString: Code = .controlCharacterInString
    /// A comma was found where a colon was expected in an object.
    public static let expectedColon: Code = .expectedColon
    /// A comma or colon was found in an object without a key.
    public static let missingKey: Code = .missingKey
    /// An object key was found that was not a string.
    public static let nonStringKey: Code = .nonStringKey
    /// A comma or object end was encountered where a value was expected.
    public static let missingValue: Code = .missingValue
    /// A trailing comma was found in an array or object. Only emitted when `strict` mode is enabled.
    public static let trailingComma: Code = .trailingComma
    /// Trailing (non-whitespace) characters found after the close
    /// of the root value.
    /// - Note: This error cannot be thrown if the parser is in streaming mode.
    public static let trailingCharacters: Code = .trailingCharacters
    /// EOF was found before the root value finished parsing.
    public static let unexpectedEOF: Code = .unexpectedEOF
    
    public let code: Code
    public let line: UInt
    public let column: UInt
    
    public init(code: Code, line: UInt, column: UInt) {
        self.code = code
        self.line = line
        self.column = column
    }
    
    public var _code: Int { return code.rawValue }
    
    public enum Code: Int {
        /// A generic syntax error.
        case invalidSyntax
        /// An invalid number.
        case invalidNumber
        /// An invalid string escape.
        case invalidEscape
        /// A unicode string escape with an invalid code point.
        case invalidUnicodeScalar
        /// A unicode string escape representing a leading surrogate without
        /// a corresponding trailing surrogate.
        case loneLeadingSurrogateInUnicodeEscape
        /// A control character in a string.
        case controlCharacterInString
        /// A comma was found where a colon was expected in an object.
        case expectedColon
        /// A comma or colon was found in an object without a key.
        case missingKey
        /// An object key was found that was not a string.
        case nonStringKey
        /// A comma or object end was encountered where a value was expected.
        case missingValue
        /// A trailing comma was found in an array or object. Only emitted when `strict` mode is enabled.
        case trailingComma
        /// Trailing (non-whitespace) characters found after the close
        /// of the root value.
        /// - Note: This error cannot be thrown if the parser is in streaming mode.
        case trailingCharacters
        /// EOF was found before the root value finished parsing.
        case unexpectedEOF
        
        public static func ~=(lhs: Code, rhs: Error) -> Bool {
            if let error = rhs as? JSONParserError {
                return lhs == error.code
            } else {
                return false
            }
        }
    }
    
    public var description: String {
        return "JSONParserError(\(code), line: \(line), column: \(column))"
    }
    
    public var hashValue: Int {
        return Int(bitPattern: line << 18) ^ Int(bitPattern: column << 4) ^ code.rawValue
    }
    
    public static func ==(lhs: JSONParserError, rhs: JSONParserError) -> Bool {
        return (lhs.code, lhs.line, lhs.column) == (rhs.code, rhs.line, rhs.column)
    }
}

private struct PeekIterator<Base: IteratorProtocol> {
    init(_ base: Base) {
        self.base = base
    }
    
    mutating func peek() -> Base.Element? {
        if let elt = peeked {
            return elt
        }
        let elt = base.next()
        peeked = .some(elt)
        return elt
    }
    
    mutating func next() -> Base.Element? {
        if let elt = peeked {
            peeked = nil
            return elt
        }
        return base.next()
    }
    
    private var base: Base
    private var peeked: Base.Element??
}

private func replace<T>(_ a: inout T, with b: T) -> T {
    var b = b
    swap(&a, &b)
    return b
}
