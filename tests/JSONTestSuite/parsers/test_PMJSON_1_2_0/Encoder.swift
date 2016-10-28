//
//  Encoder.swift
//  PMJSON
//
//  Created by Kevin Ballard on 2/1/16.
//  Copyright © 2016 Postmates.
//
//  Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
//  http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
//  <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
//  option. This file may not be copied, modified, or distributed
//  except according to those terms.
//

extension JSON {
    /// Encodes a `JSON` to a `String`.
    /// - Parameter json: The `JSON` to encode.
    /// - Parameters options: Options that controls JSON encoding. Defaults to no options. See `JSONEncoderOptions` for details.
    /// - Returns: A `String` with the JSON representation of *json*.
    public static func encodeAsString(_ json: JSON, options: JSONEncoderOptions = []) -> String {
        var s = ""
        encode(json, to: &s, options: options)
        return s
    }
    
    @available(*, deprecated, message: "Use JSON.encodeAsString(_:options:) instead")
    public static func encodeAsString(_ json: JSON, pretty: Bool) -> String {
        return encodeAsString(json, options: JSONEncoderOptions(pretty: pretty))
    }
    
    /// Encodes a `JSON` to an output stream.
    /// - Parameter json: The `JSON` to encode.
    /// - Parameter stream: The output stream to write the encoded JSON to.
    /// - Parameters options: Options that controls JSON encoding. Defaults to no options. See `JSONEncoderOptions` for details.
    public static func encode<Target: TextOutputStream>(_ json: JSON, to stream: inout Target, options: JSONEncoderOptions = []) {
        encode(json, to: &stream, indent: options.pretty ? 0 : nil)
    }
    
    @available(*, deprecated, message: "Use JSON.encode(_:to:options:) instead")
    public static func encode<Target: TextOutputStream>(_ json: JSON, to stream: inout Target, pretty: Bool) {
        encode(json, to: &stream, options: JSONEncoderOptions(pretty: pretty))
    }
    
    @available(*, deprecated, renamed: "encode(_:to:pretty:)")
    public static func encode<Target: TextOutputStream>(_ json: JSON, toStream stream: inout Target, pretty: Bool) {
        encode(json, to: &stream, options: JSONEncoderOptions(pretty: pretty))
    }
    
    private static func encode<Target: TextOutputStream>(_ json: JSON, to stream: inout Target, indent: Int?) {
        switch json {
        case .null: encodeNull(&stream)
        case .bool(let b): encodeBool(b, toStream: &stream)
        case .int64(let i): encodeInt64(i, toStream: &stream)
        case .double(let d): encodeDouble(d, toStream: &stream)
        case .string(let s): encodeString(s, toStream: &stream)
        case .object(let obj): encodeObject(obj, toStream: &stream, indent: indent)
        case .array(let ary): encodeArray(ary, toStream: &stream, indent: indent)
        }
    }
    
    private static func encodeNull<Target: TextOutputStream>(_ stream: inout Target) {
        stream.write("null")
    }
    
    private static func encodeBool<Target: TextOutputStream>(_ value: Bool, toStream stream: inout Target) {
        stream.write(value ? "true" : "false")
    }
    
    private static func encodeInt64<Target: TextOutputStream>(_ value: Int64, toStream stream: inout Target) {
        stream.write(String(value))
    }
    
    private static func encodeDouble<Target: TextOutputStream>(_ value: Double, toStream stream: inout Target) {
        stream.write(String(value))
    }
    
    private static func encodeString<Target: TextOutputStream>(_ value: String, toStream stream: inout Target) {
        stream.write("\"")
        let scalars = value.unicodeScalars
        var start = scalars.startIndex
        let end = scalars.endIndex
        var idx = start
        while idx < scalars.endIndex {
            let s: String
            let c = scalars[idx]
            switch c {
            case "\\": s = "\\\\"
            case "\"": s = "\\\""
            case "\n": s = "\\n"
            case "\r": s = "\\r"
            case "\t": s = "\\t"
            case "\u{8}": s = "\\b"
            case "\u{C}": s = "\\f"
            case "\0"..<"\u{10}":
                s = "\\u000\(String(c.value, radix: 16, uppercase: true))"
            case "\u{10}"..<" ":
                s = "\\u00\(String(c.value, radix: 16, uppercase: true))"
            default:
                idx = scalars.index(after: idx)
                continue
            }
            if idx != start {
                stream.write(String(scalars[start..<idx]))
            }
            stream.write(s)
            idx = scalars.index(after: idx)
            start = idx
        }
        if start != end {
            String(scalars[start..<end]).write(to: &stream)
        }
        stream.write("\"")
    }
    
    private static func encodeObject<Target: TextOutputStream>(_ object: JSONObject, toStream stream: inout Target, indent: Int?) {
        let indented = indent.map({$0+1})
        if let indent = indented {
            stream.write("{\n")
            writeIndent(indent, toStream: &stream)
        } else {
            stream.write("{")
        }
        var first = true
        for (key, value) in object {
            if first {
                first = false
            } else if let indent = indented {
                stream.write(",\n")
                writeIndent(indent, toStream: &stream)
            } else {
                stream.write(",")
            }
            encodeString(key, toStream: &stream)
            stream.write(indented != nil ? ": " : ":")
            encode(value, to: &stream, indent: indented)
        }
        if let indent = indent {
            stream.write("\n")
            writeIndent(indent, toStream: &stream)
        }
        stream.write("}")
    }
    
    private static func encodeArray<Target: TextOutputStream>(_ array: JSONArray, toStream stream: inout Target, indent: Int?) {
        let indented = indent.map({$0+1})
        if let indent = indented {
            stream.write("[\n")
            writeIndent(indent, toStream: &stream)
        } else {
            stream.write("[")
        }
        var first = true
        for elt in array {
            if first {
                first = false
            } else if let indent = indented {
                stream.write(",\n")
                writeIndent(indent, toStream: &stream)
            } else {
                stream.write(",")
            }
            encode(elt, to: &stream, indent: indented)
        }
        if let indent = indent {
            stream.write("\n")
            writeIndent(indent, toStream: &stream)
        }
        stream.write("]")
    }
    
    private static func writeIndent<Target: TextOutputStream>(_ indent: Int, toStream stream: inout Target) {
        for _ in stride(from: 4, through: indent, by: 4) {
            stream.write("        ")
        }
        switch indent % 4 {
        case 1: stream.write("  ")
        case 2: stream.write("    ")
        case 3: stream.write("      ")
        default: break
        }
    }
}

public struct JSONEncoderOptions {
    /// If `true`, the output is formatted with whitespace to be easier to read.
    /// If `false`, the output omits any unnecessary whitespace.
    ///
    /// The default value is `false`.
    public var pretty: Bool = false
    
    /// Returns a new `JSONEncoderOptions` with default values.
    public init() {}
    
    /// Returns a new `JSONEncoderOptions`.
    /// - Parameter pretty: Whether the output should be formatted nicely. Defaults to `false`.
    public init(pretty: Bool = false) {
        self.pretty = pretty
    }
}

extension JSONEncoderOptions: ExpressibleByArrayLiteral {
    public enum Element {
        /// Formats the output with whitespace to be easier to read.
        /// - SeeAlso: `JSONEncoderOptions.pretty`.
        case pretty
    }
    
    public init(arrayLiteral elements: Element...) {
        for elt in elements {
            switch elt {
            case .pretty: pretty = true
            }
        }
    }
}
