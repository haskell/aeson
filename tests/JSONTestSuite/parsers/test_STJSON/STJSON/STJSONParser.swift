//
//  JSONParser.swift
//  JSON
//
//  Created by Nicolas Seriot on 12/07/16.
//  Copyright © 2016 Nicolas Seriot. All rights reserved.
//

import Foundation

public struct STJSONParser {

    public struct Options: OptionSet {
        // https://oleb.net/blog/2016/09/swift-option-sets/
        
        public let rawValue: Int
        
        public init(rawValue:Int) {
            self.rawValue = rawValue
        }
        
        static let useUnicodeReplacementCharacter = Options(rawValue: 1)
    }

    enum ASCIIByte : UInt8 {
        case objectOpen = 0x7B     // {
        case tab = 0x09            // \t
        case newline = 0x0A        // \n
        case space = 0x20          // " "
        case carriageReturn = 0x0D // \r
        case doubleQuote = 0x22    // "
        case semiColon = 0x3A      // :
        case objectClose = 0x7D    // }
        case comma = 0x2C          // ,
        case arrayOpen = 0x5B      // [
        case arrayClose = 0x5D     // ]
        case slash = 0x2F          // /
        case backSlash = 0x5C      // \
        case plus = 0x2B           // +
        case minus = 0x2D          // -
        case dot = 0x2E            // .
        case zero = 0x30           // 0
        case one = 0x31            // 1
        case nine = 0x39           // 9
        case utf8BOMByte1 = 0xEF
        case utf8BOMByte2 = 0xBB
        case utf8BOMByte3 = 0xBF
        case A = 0x41
        case E = 0x45
        case F = 0x46
        case Z = 0x5A
        case a = 0x61
        case z = 0x7A
        case e = 0x65
        case f = 0x66
        case l = 0x6C
        case n = 0x6e
        case r = 0x72
        case s = 0x73
        case t = 0x74
        case u = 0x75
    }
    
    let data : Data
    var i = 0
    let dataLength : Int
    var parserDepth = 0
    var maxParserDepth : Int
    let REPLACEMENT_STRING = "\u{FFFD}"
    var options : Options
    
    init(data:Data, maxParserDepth : Int = 512, options:Options = []) {
        self.maxParserDepth = maxParserDepth
        self.data = data
        self.dataLength = data.count
        self.options = options
    }
    
    func printRemainingString() {
        let range = Range(uncheckedBounds: (i, data.count))
        let remainingData = data.subdata(in:range)
        guard let remainingString = String(data:remainingData, encoding: .utf8) else {
            print("-- can't print remaining string")
            return
        }
        print("-- REMAINING STRING FROM \(i): \(remainingString)")
    }
    
    func read() -> UInt8? {
        guard i < dataLength else { /* print("can't read at index \(i)"); */ return nil }
        return data[i]
    }
    
    mutating func readAndMoveByteEither(_ a:[ASCIIByte]) -> UnicodeScalar? {
        
        guard let byte = read() else { return nil }
        
        for b in a {
            if byte == b.rawValue {
                i = i+1
                return UnicodeScalar(byte)
            }
        }
        
        return nil
    }
    
    mutating func readAndMove(_ b:ASCIIByte) -> Bool {
        
        guard let byte = read()
            , byte == b.rawValue else { return false }
        
        i = i+1
        
        return true
    }
    
    mutating func readAndMoveInByteRange(_ from:ASCIIByte, _ to:ASCIIByte) -> UnicodeScalar? {
        
        guard let byte = read()
            , byte >= from.rawValue && byte <= to.rawValue else { return nil }
        
        i = i+1
        
        return UnicodeScalar(byte)
    }
    
    mutating func readAndMoveHexadecimalDigit() -> UInt8? {
        
        guard let byte = read()
            , readAndMoveInByteRange(.zero, .nine) != nil
                || readAndMoveInByteRange(.A, .F) != nil
                || readAndMoveInByteRange(.a, .f) != nil else {
                    return nil
        }
        
        return byte
    }
    
    mutating func readAndMoveWhitespace() -> Bool {
        return readAndMove(.tab)
            || readAndMove(.newline)
            || readAndMove(.carriageReturn)
            || readAndMove(.space)
    }
    
    mutating func readAndMoveAcceptableByte() -> Bool {
        
        guard let byte = read()
            ,byte != ASCIIByte.doubleQuote.rawValue && byte != ASCIIByte.backSlash.rawValue && byte > 0x1F else {
                return false
        }
        
        i = i + 1
        
        return true
    }
    
    mutating func readArray() throws -> [Any] {
        
        while readAndMoveWhitespace() {}
        
        guard readAndMove(.arrayOpen) else {
            throw JSONError.expectedArrayOpen(i:i)
        }
        parserDepth += 1
        guard parserDepth <= maxParserDepth else { throw JSONError.maxParserDepthReached(depth:maxParserDepth) }

        var a : [Any] = []

        while readAndMoveWhitespace() {}
        
        if readAndMove(.arrayClose) {
            parserDepth -= 1
            return a
        }
        
        if let v = try readValue() {
            a.append(v)
            
            while readAndMoveWhitespace() {}
            
            while readAndMove(.comma) {
                guard let v = try readValue() else {
                    throw JSONError.expectedValue(i:i)
                }
                a.append(v)
                
                while readAndMoveWhitespace() {}
            }
        }
        
        guard readAndMove(.arrayClose) else {
            throw JSONError.expectedArrayClose(i:i)
        }
        parserDepth -= 1
        
        return a
    }
    
    func unescape(_ byte:UInt8) throws -> UInt8 {
        if byte == ASCIIByte.backSlash.rawValue
            || byte == ASCIIByte.doubleQuote.rawValue
            || byte == ASCIIByte.slash.rawValue {
            return byte
        }
        
        if byte == UInt8(ascii: "b") { return 0x08 } // \b backspace
        if byte == UInt8(ascii: "f") { return 0x0C } // \f form feed
        if byte == UInt8(ascii: "n") { return 0x0A } // \n line feed
        if byte == UInt8(ascii: "r") { return 0x0D } // \r carriage return
        if byte == UInt8(ascii: "t") { return 0x09 } // \t tabulation
        
        throw JSONError.expectedCharacterToBeUnescaped(i:i)
    }

    /*
    func isValidCodepoint(codepoint cp:Int) -> Bool {
        // http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf
        // Table 3-7. Well-Formed UTF-8 Byte Sequences
        return (cp >= 0 && cp <= 0xD7FF) || (cp >= 0xE000 && cp <= 0x10FFFF)
    }
    */
    
    func isHighSurrogate(codepoint cp:Int) -> Bool {
        return cp >= 0xD800 && cp <= 0xDBFF
    }
    
    func isLowSurrogate(codepoint cp:Int) -> Bool {
        return cp >= 0xDC00 && cp <= 0xDFFF
    }
    
    mutating func readAndMoveUEscapedCodepoint() -> (String, Int)? {
        guard readAndMove(.u) else { return nil }
        
        guard let b1 = readAndMoveHexadecimalDigit() else { /*print("** expected hexadecimal digit");*/ return nil }
        guard let b2 = readAndMoveHexadecimalDigit() else { /*print("** expected hexadecimal digit");*/ return nil }
        guard let b3 = readAndMoveHexadecimalDigit() else { /*print("** expected hexadecimal digit");*/ return nil }
        guard let b4 = readAndMoveHexadecimalDigit() else { /*print("** expected hexadecimal digit");*/ return nil }
        
        guard let s = String(bytes: [b1, b2, b3, b4], encoding: .utf8) else {
            fatalError("** cannot convert \([b1, b2, b3, b4]) into string")
        }
        
        return (s, strtol(s, nil, 16))
    }
    
    mutating func readAndMoveEscapedCodepointOrSurrogates() throws -> String? {
        
        let useReplacementString = self.options.contains(.useUnicodeReplacementCharacter)
        
        /*
         http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-262.pdf
         -> 6.1.4 The String Type
         // A code unit in the range 0 to 0xD7FF or in the range 0xE000 to 0xFFFF is interpreted as a code point with the same value.
         * A sequence of two code units, where the first code unit c1 is in the range 0xD800 to 0xDBFF and the second code unit c2
         is in the range 0xDC00 to 0xDFFF, is a surrogate pair and is interpreted as a code point with the value (c1 ‑ 0xD800) ×
         0x400 + (c2 ‑ 0xDC00) + 0x10000. (See 10.1.2)
         * A code unit that is in the range 0xD800 to 0xDFFF, but is not part of a surrogate pair, is interpreted as a code point with
         the same value.
         */
        
        guard let (s1, c1) = readAndMoveUEscapedCodepoint() else {
            return nil
        }

        // valid codepoint -> return
        if let u = UnicodeScalar(c1) {
            return "\(u)"
        }
        
        // invalid codepoint must be high surrogate, or error
        guard isHighSurrogate(codepoint: c1) else {
            return useReplacementString ? REPLACEMENT_STRING : "\\u\(s1)"
        }
        
        // look for second surrogate escape character, or error
        guard readAndMove(.backSlash) else {
            return useReplacementString ? REPLACEMENT_STRING : "\\u\(s1)"
        }
        
        // read second codepoint
        guard let (s2, c2) = readAndMoveUEscapedCodepoint() else {
            // or escaped sequence
            let x = try readAndMoveEscapedSequence()
            return useReplacementString ? REPLACEMENT_STRING + "\(x)" : "\\u\(s1)\(x)"
        }
        
        // second codepoint must be low surrogate, or error
        guard isLowSurrogate(codepoint: c2) else {
            return useReplacementString ? REPLACEMENT_STRING + REPLACEMENT_STRING : "\\u\(s1)\\u\(s2)"
        }
        
        let finalCodepoint = 0x400 + 0x2400 + (c1 - 0xD800) + (c2 - 0xDC00) + 0x10000
        guard let u = UnicodeScalar(finalCodepoint) else {
            return useReplacementString ? REPLACEMENT_STRING + REPLACEMENT_STRING : "\\u\(s1)\\u\(s2)"
        }

        return "\(u)"
    }
    
    mutating func readAndMoveEscapedSequence() throws -> String {
        
        if let s = try readAndMoveEscapedCodepointOrSurrogates() {
            return s
        }
        
        guard let b = read() else {
            throw JSONError.expectedCharacter(i:i)
        }
        
        i = i + 1
        
        let unescaped = try unescape(b)
        
        let data = Data(bytes: [unescaped])
        
        guard let s = String(data:data, encoding:.utf8) else {
            fatalError()
        }
        
        return s
    }
    
    mutating func readString() throws -> String {
        
        if readAndMove(.doubleQuote) == false {
            throw JSONError.expectedDoubleQuote(i:i)
        }
        
        var s : String = ""
        
        while readAndMove(.doubleQuote) == false {
            
            guard read() != nil else {
                throw JSONError.expectedCharacter(i:i)
            }
            
            if readAndMove(.backSlash) {
                
                let x = try readAndMoveEscapedSequence()
                s.append(x)
                
            } else if readAndMoveAcceptableByte() {
                
                let start = i-1
                
                while readAndMoveAcceptableByte() {
                    ()
                }
                
                let range = Range(uncheckedBounds: (start, i))
                
                let subdata = data.subdata(in: range)
                
                guard let substring = String(data:subdata, encoding:.utf8) else {
                    throw JSONError.cannotBuildStringFromData(i:i)
                }
                
                s.append(substring)
            } else {
                throw JSONError.expectedAcceptableCodepointOrEscapedSequence(i:i)
            }
        }
        
        return s
    }
    
    mutating func readObject() throws -> [String:Any] {
        
        guard readAndMove(.objectOpen) else {
            throw JSONError.expectedObjectOpen(i:i)
        }
        parserDepth += 1
        guard parserDepth <= maxParserDepth else { throw JSONError.maxParserDepthReached(depth:maxParserDepth) }
        
        var d : [String:Any] = [:]
        
        while readAndMoveWhitespace() {}
        
        if readAndMove(.objectClose) {
            parserDepth -= 1
            return d
        }
        
        repeat {
            
            while readAndMoveWhitespace() {}
            
            let s = try readString()
            
            while readAndMoveWhitespace() {}
            
            guard readAndMove(.semiColon) else {
                throw JSONError.expectedSemicolon(i:i)
            }
            
            guard let v = try readValue() else {
                throw JSONError.expectedValue(i:i)
            }
            
            d[s] = v
            
            while readAndMoveWhitespace() {}
            
        } while readAndMove(.comma)
        
        guard readAndMove(.objectClose) else {
            throw JSONError.expectedObjectClose(i:i)
        }
        parserDepth -= 1
        
        return d
    }
    
    func throwIfStartsWithUTF16BOM() throws {
        let BOM_LENGTH = 2
        
        guard dataLength >= BOM_LENGTH else { return }

        let UTF_16_BE = Data(bytes: [0xFE, 0xFF])
        let UTF_16_LE = Data(bytes: [0xFF, 0xFE])
        
        try data.withUnsafeBytes { (p:UnsafePointer<UInt8>) -> Void in
            let BOM = Data(buffer: UnsafeBufferPointer(start: p, count: BOM_LENGTH))
            
            if BOM == UTF_16_BE { throw JSONError.foundBOMForUnsupportdEncodingUTF16BE }
            if BOM == UTF_16_LE { throw JSONError.foundBOMForUnsupportdEncodingUTF16LE }
        }
    }
    
    func throwIfStartsWithUTF32BOM() throws {
        let BOM_LENGTH = 4

        guard dataLength >= BOM_LENGTH else { return }

        let UTF_32_BE = Data(bytes: [0x00, 0x00, 0xFE, 0xFF])
        let UTF_32_LE = Data(bytes: [0xFF, 0xFE, 0x00, 0x00])
        
        try data.withUnsafeBytes { (p:UnsafePointer<UInt8>) -> Void in
            let BOM = Data(buffer: UnsafeBufferPointer(start: p, count: BOM_LENGTH))
            
            if BOM == UTF_32_BE { throw JSONError.foundBOMForUnsupportdEncodingUTF32BE }
            if BOM == UTF_32_LE { throw JSONError.foundBOMForUnsupportdEncodingUTF32LE }
        }
    }
    
    mutating func parse() throws -> Any? {
        
        // throw if a UTF-16 or UTF-32 BOM is found
        // this is the only place where STJSON does not follow RFC 7159
        // which supports UTF-16 anf UTF-32, optionnaly preceeded by a BOM
        // STJSON only supports UTF-8
        try throwIfStartsWithUTF16BOM()
        try throwIfStartsWithUTF32BOM()
        
        // skip UTF-8 BOM if present (EF BB BF)
        if readAndMove(.utf8BOMByte1) {
            guard readAndMove(.utf8BOMByte2) else { return nil}
            guard readAndMove(.utf8BOMByte3) else { return nil}
        }
        
        let o = try readValue()
        
        while readAndMoveWhitespace() {}
        guard read() == nil else {
            throw JSONError.extraData(i:i)
        }
        return o
    }
    
    func myDouble(_ s:String) -> Double? {
        
        return s.withCString() { startPointer -> Double in
            
            var doubleEndPointer : UnsafeMutablePointer<Int8>? = nil
            
            return strtod(startPointer, &doubleEndPointer)
        }
    }
    
//    func myInt(_ s:String) -> Int? {
//        
//        return s.withCString() { startPointer -> Int in
//            
//            var intEndPointer : UnsafeMutablePointer<Int8>? = nil
//            
//            return strtol(startPointer, &intEndPointer, 10)
//        }
//    }
    
    mutating func readValue() throws -> Any? {
        
        while readAndMoveWhitespace() {}
        
        guard let byte = read() else {
            throw JSONError.cannotReadByte(i:i)
        }
        
        switch(byte) {
        case ASCIIByte.arrayOpen.rawValue:
            return try readArray()
        case ASCIIByte.doubleQuote.rawValue:
            return try readString()
        case ASCIIByte.objectOpen.rawValue:
            return try readObject()
        case ASCIIByte.t.rawValue:
            let start_pos = i
            if readAndMove(.t)
                && readAndMove(.r)
                && readAndMove(.u)
                && readAndMove(.e) {
                return true
            }
            throw JSONError.foundGarbage(i: start_pos)
        case ASCIIByte.f.rawValue:
            let start_pos = i
            if readAndMove(.f)
                && readAndMove(.a)
                && readAndMove(.l)
                && readAndMove(.s)
                && readAndMove(.e) {
                return false
            }
            throw JSONError.foundGarbage(i: start_pos)
        case ASCIIByte.n.rawValue:
            let start_pos = i
            if readAndMove(.n)
                && readAndMove(.u)
                && readAndMove(.l)
                && readAndMove(.l) {
                return NSNull()
            }
            throw JSONError.foundGarbage(i: start_pos)
        default:
            guard let (isADouble, n) = try readNumber() else {
                throw JSONError.expectedNumber(i: i)
            }
            
            guard let d = myDouble(n) else {
                print("n is not a double")
                return nil
            }
            
            if d.isInfinite {
                print("d is infinite")
                return nil
            }
            
            if d.isNaN {
                print("d in NaN")
                return nil
            }
            
            if isADouble {
                return d
            }
            
            if d > Double(Int.max) {
                return n // Int cannot represent n, so we return the string
            }
            
            if d < Double(Int.min) {
                return n // Int cannot represent n, so we return the string
            }
            
            return Int(d)
        }
    }
    
    enum JSONError: Error {
        case cannotReadByte(i:Int)
        case expectedDigit(i:Int)
        case expectedCharacterToBeUnescaped(i:Int)
        case expectedValue(i:Int)
        case expectedString(i:Int)
        case expectedNumber(i:Int)
        case expectedSemicolon(i:Int)
        case expectedObjectOpen(i:Int)
        case expectedObjectContent(i:Int)
        case expectedObjectClose(i:Int)
        case expectedArrayOpen(i:Int)
        case expectedArrayClose(i:Int)
        case expectedDoubleQuote(i:Int)
        case expectedCharacter(i:Int)
        case cannotBuildStringFromData(i:Int)
        case expectedAcceptableCodepointOrEscapedSequence(i:Int)
        case cannotReadInt(i:Int)
        case extraData(i:Int)
        case maxParserDepthReached(depth:Int)
        case foundGarbage(i:Int)
        case expectedHighSurrogate(i:Int)
        case expectedLowSurrogate(i:Int)
        case foundSurrogatesWithInvalidCodepoint(i:Int)
        case foundBOMForUnsupportdEncodingUTF16BE
        case foundBOMForUnsupportdEncodingUTF16LE
        case foundBOMForUnsupportdEncodingUTF32BE
        case foundBOMForUnsupportdEncodingUTF32LE
    }
    
    mutating func readNumber() throws -> (Bool, String)? {
        
        var isADouble = false
        
        var s = ""
        
        if readAndMove(.minus) {
            s.append("-")
        }
        
        guard let b = read()
            , b >= ASCIIByte.zero.rawValue && b <= ASCIIByte.nine.rawValue else { return nil }
        
        if readAndMove(.zero) {
            s.append("0")
        } else if let d = readAndMoveInByteRange(.one, .nine) {
            s.append(String(d))
            while let d = readAndMoveInByteRange(.zero, .nine) {
                s.append(String(d))
            }
        }
        
        if readAndMove(.dot) {
            isADouble = true
            
            s.append(".")
            
            var digitFound = false
            
            while let d = readAndMoveInByteRange(.zero, .nine) {
                digitFound = true
                s.append(String(d))
            }
            
            guard digitFound else {
                throw JSONError.expectedDigit(i: i)
            }
        }
        
        if let x = readAndMoveByteEither([.e, .E]) {
            isADouble = true

            s.append(String(x))
            
            if let x = readAndMoveByteEither([.plus, .minus]) {
                s.append(String(x))
            }
            
            var digitFound = false
            
            while let d = readAndMoveInByteRange(.zero, .nine) {
                digitFound = true
                s.append(String(d))
            }
            
            guard digitFound else {
                throw JSONError.expectedDigit(i: i)
            }
        }
        
        return (isADouble, s)
    }
}
