//
//  JSONParser.swift
//  Freddy
//
//  Created by John Gallagher on 4/18/15.
//  Copyright © 2015 Big Nerd Ranch. Licensed under MIT.
//

import Foundation

private struct Literal {
    static let BACKSLASH     = UInt8(ascii: "\\")
    static let BACKSPACE     = UInt8(ascii: "\u{0008}")
    static let COLON         = UInt8(ascii: ":")
    static let COMMA         = UInt8(ascii: ",")
    static let DOUBLE_QUOTE  = UInt8(ascii: "\"")
    static let FORMFEED      = UInt8(ascii: "\u{000c}")
    static let LEFT_BRACE    = UInt8(ascii: "{")
    static let LEFT_BRACKET  = UInt8(ascii: "[")
    static let MINUS         = UInt8(ascii: "-")
    static let NEWLINE       = UInt8(ascii: "\n")
    static let PERIOD        = UInt8(ascii: ".")
    static let PLUS          = UInt8(ascii: "+")
    static let RETURN        = UInt8(ascii: "\r")
    static let RIGHT_BRACE   = UInt8(ascii: "}")
    static let RIGHT_BRACKET = UInt8(ascii: "]")
    static let SLASH         = UInt8(ascii: "/")
    static let SPACE         = UInt8(ascii: " ")
    static let TAB           = UInt8(ascii: "\t")

    static let a = UInt8(ascii: "a")
    static let b = UInt8(ascii: "b")
    static let c = UInt8(ascii: "c")
    static let d = UInt8(ascii: "d")
    static let e = UInt8(ascii: "e")
    static let f = UInt8(ascii: "f")
    static let l = UInt8(ascii: "l")
    static let n = UInt8(ascii: "n")
    static let r = UInt8(ascii: "r")
    static let s = UInt8(ascii: "s")
    static let t = UInt8(ascii: "t")
    static let u = UInt8(ascii: "u")

    static let A = UInt8(ascii: "A")
    static let B = UInt8(ascii: "B")
    static let C = UInt8(ascii: "C")
    static let D = UInt8(ascii: "D")
    static let E = UInt8(ascii: "E")
    static let F = UInt8(ascii: "F")

    static let zero  = UInt8(ascii: "0")
    static let one   = UInt8(ascii: "1")
    static let two   = UInt8(ascii: "2")
    static let three = UInt8(ascii: "3")
    static let four  = UInt8(ascii: "4")
    static let five  = UInt8(ascii: "5")
    static let six   = UInt8(ascii: "6")
    static let seven = UInt8(ascii: "7")
    static let eight = UInt8(ascii: "8")
    static let nine  = UInt8(ascii: "9")
}

private let ParserMaximumDepth = 512


/// A pure Swift JSON parser. This parser is much faster than the
/// `NSJSONSerialization`-based parser (due to the overhead of having to
/// dynamically cast the Objective-C objects to determine their type); however,
/// it is much newer and has restrictions that the `NSJSONSerialization` parser
/// does not. Two restrictions in particular are that it requires UTF-8 data as
/// input and it does not allow trailing commas in arrays or dictionaries.
public struct JSONParser {

    private enum Sign: Int {
        case Positive = 1
        case Negative = -1
    }

    private let input: UnsafeBufferPointer<UInt8>
    private let owner: Any?
    private var loc = 0
    private var depth = 0

    private init<T>(buffer: UnsafeBufferPointer<UInt8>, owner: T) {
        self.input = buffer
        self.owner = owner
    }

    /// Decode the root element of the `JSON` stream. This may be any fragment
    /// or a structural element, per RFC 7159.
    ///
    /// The beginning bytes are used to determine the stream's encoding.
    /// `JSONParser` currently only supports UTF-8 encoding, with or without
    /// a byte-order mark.
    ///
    /// - throws: `JSONParser.Error` for any decoding failures, including a
    ///   source location if needed.
    public mutating func parse() throws -> JSON {
        try guardAgainstUnsupportedEncodings()
        let value = try parseValue()
        skipWhitespace()
        guard loc == input.count else {
            throw Error.EndOfStreamGarbage(offset: loc)
        }
        return value
    }

    private mutating func parseValue() throws -> JSON {
        guard depth <= ParserMaximumDepth else {
            throw Error.ExceededNestingLimit(offset: loc)
        }
        
        guard input.count > 0 else {
            throw Error.EndOfStreamUnexpected
        }

        advancing: while loc < input.count {
            do {
                switch input[loc] {
                case Literal.LEFT_BRACKET:
                    depth += 1
                    defer { depth -= 1 }
                    return try decodeArray()
                    
                case Literal.LEFT_BRACE:
                    depth += 1
                    defer { depth -= 1 }
                    return try decodeObject()
                    
                case Literal.DOUBLE_QUOTE:
                    return try decodeString()
                    
                case Literal.f:
                    return try decodeFalse()
                    
                case Literal.n:
                    return try decodeNull()
                    
                case Literal.t:
                    return try decodeTrue()

                case Literal.MINUS:
                    return try decodeIntegralValue(NumberParser(loc: loc, input: input, state: .LeadingMinus))

                case Literal.zero:
                    return try decodeIntegralValue(NumberParser(loc: loc, input: input, state: .LeadingZero))

                case Literal.one...Literal.nine:
                    return try decodeIntegralValue(NumberParser(loc: loc, input: input, state: .PreDecimalDigits))

                case Literal.SPACE, Literal.TAB, Literal.RETURN, Literal.NEWLINE:
                    loc = loc.successor()
                    
                default:
                    break advancing
                }
            } catch let InternalError.NumberOverflow(offset: start) {
                return try decodeNumberAsString(start)
            }
        }
        
        throw Error.ValueInvalid(offset: loc, character: UnicodeScalar(input[loc]))
    }

    private mutating func skipWhitespace() {
        while loc < input.count {
            switch input[loc] {
            case Literal.SPACE, Literal.TAB, Literal.RETURN, Literal.NEWLINE:
                loc = loc.successor()

            default:
                return
            }
        }
    }

    private mutating func guardAgainstUnsupportedEncodings() throws {
        let header = input.prefix(4)
        let encodingPrefixInformation = JSONEncodingDetector.detectEncoding(header)
        guard JSONEncodingDetector.supportedEncodings.contains(encodingPrefixInformation.encoding) else {
            throw Error.InvalidUnicodeStreamEncoding(detectedEncoding: encodingPrefixInformation.encoding)
        }
        loc = loc.advancedBy(encodingPrefixInformation.byteOrderMarkLength)
    }

    private mutating func decodeNull() throws -> JSON {
        guard loc.advancedBy(3, limit: input.count) != input.count else {
            throw Error.LiteralNilMisspelled(offset: loc)
        }

        if     input[loc+1] != Literal.u
            || input[loc+2] != Literal.l
            || input[loc+3] != Literal.l {
                throw Error.LiteralNilMisspelled(offset: loc)
        }

        loc += 4
        return .Null
    }

    private mutating func decodeTrue() throws -> JSON {
        guard loc.advancedBy(3, limit: input.count) != input.count else {
            throw Error.LiteralTrueMisspelled(offset: loc)
        }

        if     input[loc+1] != Literal.r
            || input[loc+2] != Literal.u
            || input[loc+3] != Literal.e {
            throw Error.LiteralTrueMisspelled(offset: loc)
        }

        loc += 4
        return .Bool(true)
    }

    private mutating func decodeFalse() throws -> JSON {
        guard loc.advancedBy(4, limit: input.count) != input.count else {
            throw Error.LiteralFalseMisspelled(offset: loc)
        }

        if     input[loc+1] != Literal.a
            || input[loc+2] != Literal.l
            || input[loc+3] != Literal.s
            || input[loc+4] != Literal.e {
            throw Error.LiteralFalseMisspelled(offset: loc)
        }

        loc += 5
        return .Bool(false)
    }

    private var stringDecodingBuffer = [UInt8]()
    private mutating func decodeString() throws -> JSON {
        let start = loc
        loc = loc.successor()
        stringDecodingBuffer.removeAll(keepCapacity: true)
        while loc < input.count {
            switch input[loc] {
            case Literal.BACKSLASH:
                loc = loc.successor()
                switch input[loc] {
                case Literal.DOUBLE_QUOTE: stringDecodingBuffer.append(Literal.DOUBLE_QUOTE)
                case Literal.BACKSLASH:    stringDecodingBuffer.append(Literal.BACKSLASH)
                case Literal.SLASH:        stringDecodingBuffer.append(Literal.SLASH)
                case Literal.b:            stringDecodingBuffer.append(Literal.BACKSPACE)
                case Literal.f:            stringDecodingBuffer.append(Literal.FORMFEED)
                case Literal.r:            stringDecodingBuffer.append(Literal.RETURN)
                case Literal.t:            stringDecodingBuffer.append(Literal.TAB)
                case Literal.n:            stringDecodingBuffer.append(Literal.NEWLINE)
                case Literal.u:
                    loc = loc.successor()
                    try readUnicodeEscape(start: loc - 2)

                    // readUnicodeEscape() advances loc on its own, so we'll `continue` now
                    // to skip the typical "advance past this character" for all the other escapes
                    continue

                default:
                    throw Error.ControlCharacterUnrecognized(offset: loc)
                }
                loc = loc.successor()

            case Literal.DOUBLE_QUOTE:
                loc = loc.successor()
                stringDecodingBuffer.append(0)

                guard let string = (stringDecodingBuffer.withUnsafeBufferPointer {
                    String.fromCString(UnsafePointer($0.baseAddress))
                }) else {
                    throw Error.UnicodeEscapeInvalid(offset: start)
                }

                return .String(string)

            case let other:
                stringDecodingBuffer.append(other)
                loc = loc.successor()
            }
        }

        throw Error.EndOfStreamUnexpected
    }

    private mutating func readCodeUnit() -> UInt16? {
        guard loc + 4 <= input.count else {
            return nil
        }
        var codeUnit: UInt16 = 0
        for c in input[loc..<loc+4] {
            let nibble: UInt16

            switch c {
            case Literal.zero...Literal.nine:
                nibble = UInt16(c - Literal.zero)

            case Literal.a...Literal.f:
                nibble = 10 + UInt16(c - Literal.a)

            case Literal.A...Literal.F:
                nibble = 10 + UInt16(c - Literal.A)

            default:
                return nil
            }
            codeUnit = (codeUnit << 4) | nibble
        }
        loc += 4
        return codeUnit
    }

    private mutating func readUnicodeEscape(start start: Int) throws {
        guard let codeUnit = readCodeUnit() else {
            throw Error.UnicodeEscapeInvalid(offset: start)
        }

        let codeUnits: [UInt16]

        if UTF16.isLeadSurrogate(codeUnit) {
            // First half of a UTF16 surrogate pair - we must parse another code unit and combine them

            // First confirm and skip over that we have another "\u"
            guard loc + 6 <= input.count && input[loc] == Literal.BACKSLASH && input[loc+1] == Literal.u else {
                throw Error.UnicodeEscapeInvalid(offset: start)
            }
            loc += 2

            // Ensure the second code unit is valid for the surrogate pair
            guard let secondCodeUnit = readCodeUnit() where UTF16.isTrailSurrogate(secondCodeUnit) else {
                throw Error.UnicodeEscapeInvalid(offset: start)
            }

            codeUnits = [codeUnit, secondCodeUnit]
        } else {
            codeUnits = [codeUnit]
        }

        let transcodeHadError = transcode(UTF16.self, UTF8.self, codeUnits.generate(), { self.stringDecodingBuffer.append($0) }, stopOnError: true)

        if transcodeHadError {
            throw Error.UnicodeEscapeInvalid(offset: start)
        }
    }

    private mutating func decodeArray() throws -> JSON {
        let start = loc
        loc = loc.successor()
        var items = [JSON]()

        while loc < input.count {
            skipWhitespace()

            if loc < input.count && input[loc] == Literal.RIGHT_BRACKET {
                loc = loc.successor()
                return .Array(items)
            }

            if !items.isEmpty {
                guard loc < input.count && input[loc] == Literal.COMMA else {
                    throw Error.CollectionMissingSeparator(offset: start)
                }
                loc = loc.successor()
            }

            items.append(try parseValue())
        }

        throw Error.EndOfStreamUnexpected
    }

    // Decoding objects can be recursive, so we have to keep more than one
    // buffer around for building up key/value pairs (to reduce allocations
    // when parsing large JSON documents).
    //
    // Rough estimate of the difference between this and using a fresh
    // [(String,JSON)] for the `pairs` variable in decodeObject() below is
    // about 12% on an iPhone 5.
    private struct DecodeObjectBuffers {
        var buffers = [[(String,JSON)]]()

        mutating func getBuffer() -> [(String,JSON)] {
            if !buffers.isEmpty {
                var buffer = buffers.removeLast()
                buffer.removeAll(keepCapacity: true)
                return buffer
            }
            return [(String,JSON)]()
        }

        mutating func putBuffer(buffer: [(String,JSON)]) {
            buffers.append(buffer)
        }
    }

    private var decodeObjectBuffers = DecodeObjectBuffers()

    private mutating func decodeObject() throws -> JSON {
        let start = loc
        loc = loc.successor()
        var pairs = decodeObjectBuffers.getBuffer()

        while loc < input.count {
            skipWhitespace()

            if loc < input.count && input[loc] == Literal.RIGHT_BRACE {
                loc = loc.successor()
                var obj = [String:JSON](minimumCapacity: pairs.count)
                for (k, v) in pairs {
                    obj[k] = v
                }
                decodeObjectBuffers.putBuffer(pairs)
                return .Dictionary(obj)
            }

            if !pairs.isEmpty {
                guard loc < input.count && input[loc] == Literal.COMMA else {
                    throw Error.CollectionMissingSeparator(offset: start)
                }
                loc = loc.successor()

                skipWhitespace()
            }

            guard loc < input.count && input[loc] == Literal.DOUBLE_QUOTE else {
                throw Error.DictionaryMissingKey(offset: start)
            }

            let key = try decodeString().string()
            skipWhitespace()

            guard loc < input.count && input[loc] == Literal.COLON else {
                throw Error.CollectionMissingSeparator(offset: start)
            }
            loc = loc.successor()

            pairs.append((key, try parseValue()))
        }

        throw Error.EndOfStreamUnexpected
    }

    private mutating func decodeIntegralValue(parser: NumberParser) throws -> JSON {
        var sign = Sign.Positive
        var parser = parser
        var value = 0

        // This would be more natural as `while true { ... }` with a meaningful .Done case,
        // but that causes compile time explosion in Swift 2.2. :-|
        while parser.state != .Done {
            switch parser.state {
            case .LeadingMinus:
                sign = .Negative
                try parser.parseNegative()

            case .LeadingZero:
                parser.parseLeadingZero()

            case .PreDecimalDigits:
                try parser.parsePreDecimalDigits { c in
                    guard case let (exponent, false) = Int.multiplyWithOverflow(10, value) else {
                        throw InternalError.NumberOverflow(offset: parser.start)
                    }
                    
                    guard case let (newValue, false) = Int.addWithOverflow(exponent, Int(c - Literal.zero)) else {
                        throw InternalError.NumberOverflow(offset: parser.start)
                    }
                    
                    value = newValue
                }

            case .Decimal, .Exponent:
                return try detectingFloatingPointErrors(parser.start) {
                    try decodeFloatingPointValue(parser, sign: sign, value: Double(value))
                }

            case .PostDecimalDigits, .ExponentSign, .ExponentDigits:
                assertionFailure("Invalid internal state while parsing number")

            case .Done:
                fatalError("impossible condition")
            }
        }

        guard case let (signedValue, false) = Int.multiplyWithOverflow(sign.rawValue, value) else {
            throw InternalError.NumberOverflow(offset: parser.start)
        }

        loc = parser.loc
        return .Int(signedValue)
    }

    private mutating func decodeFloatingPointValue(parser: NumberParser, sign: Sign, value: Double) throws -> JSON {
        var parser = parser
        var value = value
        var exponentSign = Sign.Positive
        var exponent = Double(0)
        var position = 0.1

        // This would be more natural as `while true { ... }` with a meaningful .Done case,
        // but that causes compile time explosion in Swift 2.2. :-|
        while parser.state != .Done {
            switch parser.state {
            case .LeadingMinus, .LeadingZero, .PreDecimalDigits:
                assertionFailure("Invalid internal state while parsing number")

            case .Decimal:
                try parser.parseDecimal()

            case .PostDecimalDigits:
                parser.parsePostDecimalDigits { c in
                    value += position * Double(c - Literal.zero)
                    position /= 10
                }

            case .Exponent:
                exponentSign = try parser.parseExponent()

            case .ExponentSign:
                try parser.parseExponentSign()

            case .ExponentDigits:
                parser.parseExponentDigits { c in
                    exponent = exponent * 10 + Double(c - Literal.zero)
                }

            case .Done:
                fatalError("impossible condition")
            }
        }

        loc = parser.loc
        return .Double(Double(sign.rawValue) * value * pow(10, Double(exponentSign.rawValue) * exponent))
    }

    private mutating func decodeNumberAsString(start: Int) throws -> JSON {
        var parser: NumberParser = {
            let state: NumberParser.State
            switch input[start] {
            case Literal.MINUS: state = .LeadingMinus
            case Literal.zero: state = .LeadingZero
            case Literal.one...Literal.nine: state = .PreDecimalDigits
            default:
                fatalError("Internal error: decodeNumber called on not-a-number")
            }
            return NumberParser(loc: start, input: input, state: state)
        }()

        stringDecodingBuffer.removeAll(keepCapacity: true)

        while true {
            switch parser.state {
            case .LeadingMinus:
                try parser.parseNegative()
                stringDecodingBuffer.append(Literal.MINUS)

            case .LeadingZero:
                parser.parseLeadingZero()
                stringDecodingBuffer.append(Literal.zero)

            case .PreDecimalDigits:
                parser.parsePreDecimalDigits { stringDecodingBuffer.append($0) }

            case .Decimal:
                try parser.parseDecimal()
                stringDecodingBuffer.append(Literal.PERIOD)

            case .PostDecimalDigits:
                parser.parsePostDecimalDigits { stringDecodingBuffer.append($0) }

            case .Exponent:
                stringDecodingBuffer.append(input[parser.loc])
                _ = try parser.parseExponent()

            case .ExponentSign:
                stringDecodingBuffer.append(input[parser.loc])
                try parser.parseExponentSign()

            case .ExponentDigits:
                parser.parseExponentDigits { stringDecodingBuffer.append($0) }

            case .Done:
                stringDecodingBuffer.append(0)
                guard let string = (stringDecodingBuffer.withUnsafeBufferPointer {
                    String.fromCString(UnsafePointer($0.baseAddress))
                }) else {
                    // Should never fail - any problems with the number string should
                    // result in thrown errors above
                    fatalError("Internal error: Invalid numeric string")
                }

                loc = parser.loc
                return .String(string)
            }
        }
    }

    private func detectingFloatingPointErrors<T>(loc: Int, @noescape _ f: () throws -> T) throws -> T {
        let flags = FE_UNDERFLOW | FE_OVERFLOW
        feclearexcept(flags)
        let value = try f()
        guard fetestexcept(flags) == 0 else {
            throw InternalError.NumberOverflow(offset: loc)
        }
        return value
    }
}

private struct NumberParser {
    enum State {
        case LeadingMinus
        case LeadingZero
        case PreDecimalDigits
        case Decimal
        case PostDecimalDigits
        case Exponent
        case ExponentSign
        case ExponentDigits
        case Done
    }

    let start: Int
    var loc = 0
    var state: State
    let input: UnsafeBufferPointer<UInt8>

    init(loc: Int, input: UnsafeBufferPointer<UInt8>, state: State) {
        assert(loc < input.count, "Invalid input to NumberParser")
        self.start = loc
        self.loc = loc
        self.input = input
        self.state = state
    }

    mutating func parseNegative() throws {
        assert(state == .LeadingMinus, "Unexpected state entering parseNegative")

        loc = loc.successor()
        guard loc < input.count else {
            throw JSONParser.Error.EndOfStreamUnexpected
        }

        switch input[loc] {
        case Literal.zero:
            state = .LeadingZero

        case Literal.one...Literal.nine:
            state = .PreDecimalDigits

        default:
            throw JSONParser.Error.NumberSymbolMissingDigits(offset: start)
        }
    }

    mutating func parseLeadingZero() {
        assert(state == .LeadingZero, "Unexpected state entering parseLeadingZero")

        loc = loc.successor()
        guard loc < input.count else {
            state = .Done
            return
        }

        guard input[loc] == Literal.PERIOD else {
            state = .Done
            return
        }

        state = .Decimal
    }

    mutating func parsePreDecimalDigits(@noescape f: (UInt8) throws -> Void) rethrows {
        assert(state == .PreDecimalDigits, "Unexpected state entering parsePreDecimalDigits")
        advancing: while loc < input.count {
            let c = input[loc]
            switch c {
            case Literal.zero...Literal.nine:
                try f(c)
                loc = loc.successor()

            case Literal.PERIOD:
                state = .Decimal
                return

            case Literal.e, Literal.E:
                state = .Exponent
                return

            default:
                break advancing
            }
        }

        state = .Done
    }

    mutating func parseDecimal() throws {
        assert(state == .Decimal, "Unexpected state entering parseDecimal")
        loc = loc.successor()
        guard loc < input.count else {
            throw JSONParser.Error.EndOfStreamUnexpected
        }

        switch input[loc] {
        case Literal.zero...Literal.nine:
            state = .PostDecimalDigits

        default:
            throw JSONParser.Error.NumberMissingFractionalDigits(offset: start)
        }
    }

    mutating func parsePostDecimalDigits(@noescape f: (UInt8) throws -> Void) rethrows {
        assert(state == .PostDecimalDigits, "Unexpected state entering parsePostDecimalDigits")

        advancing: while loc < input.count {
            let c = input[loc]
            switch c {
            case Literal.zero...Literal.nine:
                try f(c)
                loc = loc.successor()

            case Literal.e, Literal.E:
                state = .Exponent
                return

            default:
                break advancing
            }
        }

        state = .Done
    }

    mutating func parseExponent() throws -> JSONParser.Sign {
        assert(state == .Exponent, "Unexpected state entering parseExponent")

        loc = loc.successor()
        guard loc < input.count else {
            throw JSONParser.Error.EndOfStreamUnexpected
        }

        switch input[loc] {
        case Literal.zero...Literal.nine:
            state = .ExponentDigits

        case Literal.PLUS:
            state = .ExponentSign

        case Literal.MINUS:
            state = .ExponentSign
            return .Negative

        default:
            throw JSONParser.Error.NumberSymbolMissingDigits(offset: start)
        }

        return .Positive
    }

    mutating func parseExponentSign() throws {
        assert(state == .ExponentSign, "Unexpected state entering parseExponentSign")
        loc = loc.successor()
        guard loc < input.count else {
            throw JSONParser.Error.EndOfStreamUnexpected
        }

        switch input[loc] {
        case Literal.zero...Literal.nine:
            state = .ExponentDigits

        default:
            throw JSONParser.Error.NumberSymbolMissingDigits(offset: start)
        }
    }

    mutating func parseExponentDigits(@noescape f: (UInt8) throws -> Void) rethrows {
        assert(state == .ExponentDigits, "Unexpected state entering parseExponentDigits")
        advancing: while loc < input.count {
            let c = input[loc]
            switch c {
            case Literal.zero...Literal.nine:
                try f(c)
                loc = loc.successor()

            default:
                break advancing
            }
        }

        state = .Done
    }
}

public extension JSONParser {

    /// Creates a `JSONParser` ready to parse UTF-8 encoded `NSData`.
    ///
    /// If the data is mutable, it is copied before parsing. The data's lifetime
    /// is extended for the duration of parsing.
    init(utf8Data inData: NSData) {
        let data = inData.copy() as! NSData
        let buffer = UnsafeBufferPointer(start: UnsafePointer<UInt8>(data.bytes), count: data.length)
        self.init(buffer: buffer, owner: data)
    }

    /// Creates a `JSONParser` from the code units represented by the `string`.
    ///
    /// The synthesized string is lifetime-extended for the duration of parsing.
    init(string: String) {
        let codePoints = string.nulTerminatedUTF8
        let buffer = codePoints.withUnsafeBufferPointer { nulTerminatedBuffer in
            // don't want to include the nul termination in the buffer - trim it off
            UnsafeBufferPointer(start: nulTerminatedBuffer.baseAddress, count: nulTerminatedBuffer.count - 1)
        }
        self.init(buffer: buffer, owner: codePoints)
    }

}

extension JSONParser: JSONParserType {

    /// Creates an instance of `JSON` from UTF-8 encoded `NSData`.
    /// - parameter data: An instance of `NSData` to parse `JSON` from.
    /// - throws: Any `JSONParser.Error` that arises during decoding.
    /// - seealso: JSONParser.parse()
    public static func createJSONFromData(data: NSData) throws -> JSON {
        var parser = JSONParser(utf8Data: data)
        return try parser.parse()
    }

}

// MARK: - Errors

extension JSONParser {

    /// Enumeration describing possible errors that occur while parsing a JSON
    /// document. Most errors include an associated `offset`, representing the
    /// offset into the UTF-8 characters making up the document where the error
    /// occurred.
    public enum Error: ErrorType {
        /// The parser ran out of data prematurely. This usually means a value
        /// was not escaped, such as a string literal not ending with a double
        /// quote.
        case EndOfStreamUnexpected
        
        /// Unexpected non-whitespace data was left around `offset` after
        /// parsing all valid JSON.
        case EndOfStreamGarbage(offset: Int)
        
        /// Too many nested objects or arrays occured at the literal started
        /// around `offset`.
        case ExceededNestingLimit(offset: Int)
        
        /// A `character` was not a valid start of a value around `offset`.
        case ValueInvalid(offset: Int, character: UnicodeScalar)
        
        /// Badly-formed Unicode escape sequence at `offset`. A Unicode escape
        /// uses the text "\u" followed by 4 hex digits, such as "\uF09F\uA684"
        /// to represent U+1F984, "UNICORN FACE".
        case UnicodeEscapeInvalid(offset: Int)
        
        /// Badly-formed control character around `offset`. JSON supports
        /// backslash-escaped double quotes, slashes, whitespace control codes,
        /// and Unicode escape sequences.
        case ControlCharacterUnrecognized(offset: Int)
        
        /// Invalid token, expected `null` around `offset`
        case LiteralNilMisspelled(offset: Int)
        
        /// Invalid token, expected `true` around `offset`
        case LiteralTrueMisspelled(offset: Int)
        
        /// Invalid token, expected `false` around `offset`
        case LiteralFalseMisspelled(offset: Int)
        
        /// Badly-formed collection at given `offset`, expected `,` or `:`
        case CollectionMissingSeparator(offset: Int)
        
        /// While parsing an object literal, a value was found without a key
        /// around `offset`. The start of a string literal was expected.
        case DictionaryMissingKey(offset: Int)
        
        /// Badly-formed number with no digits around `offset`. After a decimal
        /// point, a number must include some number of digits.
        case NumberMissingFractionalDigits(offset: Int)
        
        /// Badly-formed number with symbols ("-" or "e") but no following
        /// digits around `offset`.
        case NumberSymbolMissingDigits(offset: Int)

        /// Supplied data is encoded in an unsupported format.
        case InvalidUnicodeStreamEncoding(detectedEncoding: JSONEncodingDetector.Encoding)
    }

    private enum InternalError: ErrorType {
        /// Attempted to parse an integer outside the range of [Int.min, Int.max]
        /// or a double outside the range of representable doubles. Note that
        /// for doubles, this could be an overflow or an underflow - we don't
        /// get enough information from Swift here to know which it is. The number
        /// causing the overflow/underflow began at `offset`.
        case NumberOverflow(offset: Int)
    }
}
