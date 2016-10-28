//
//  ObjectiveC.swift
//  PMJSON
//
//  Created by Kevin Ballard on 10/9/15.
//  Copyright © 2016 Postmates.
//
//  Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
//  http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
//  <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
//  option. This file may not be copied, modified, or distributed
//  except according to those terms.
//

#if os(iOS) || os(OSX) || os(watchOS) || os(tvOS)
    
    import Foundation
    
    extension JSON {
        /// Decodes a `Data` as JSON.
        /// - Note: Invalid unicode sequences in the data are replaced with U+FFFD.
        /// - Parameter data: The data to decode. Must be either UTF-8, or UTF-16 with a BOM.
        /// - Parameter options: Options that controls JSON parsing. Defaults to no options. See `JSONOptions` for details.
        /// - Returns: A `JSON` value.
        /// - Throws: `JSONParserError` if the data does not contain valid JSON.
        public static func decode(_ data: Data, options: JSONOptions = []) throws -> JSON {
            if let endian = UTF16Decoder.checkBOM(for: data) {
                return try JSON.decode(UTF16Decoder(data: data, endian: endian))
            } else {
                return try JSON.decode(UTF8Decoder(data: data), options: options)
            }
        }
        
        @available(*, deprecated, message: "Use JSON.decode(_:options:) instead")
        public static func decode(_ data: Data, strict: Bool) throws -> JSON {
            return try JSON.decode(data, options: [.strict])
        }
        
        /// Encodes a `JSON` to a `Data`.
        /// - Parameter json: The `JSON` to encode.
        /// - Parameter options: Options that controls JSON encoding. Defaults to no options. See `JSONEncoderOptions` for details.
        /// - Returns: An `NSData` with the JSON representation of *json*.
        public static func encodeAsData(_ json: JSON, options: JSONEncoderOptions = []) -> Data {
            struct Output: TextOutputStream {
                // NB: We use NSMutableData instead of Data because it's significantly faster as of Xcode 8b3
                var data = NSMutableData()
                mutating func write(_ string: String) {
                    let oldLen = data.length
                    data.increaseLength(by: string.utf8.count)
                    let ptr = data.mutableBytes.assumingMemoryBound(to: UInt8.self) + oldLen
                    for (i, x) in string.utf8.enumerated() {
                        ptr[i] = x
                    }
                }
            }
            var output = Output()
            JSON.encode(json, to: &output, options: options)
            return output.data as Data
        }
        
        @available(*, deprecated, message: "Use JSON.encodeAsData(_:options:) instead")
        public static func encodeAsData(_ json: JSON, pretty: Bool) -> Data {
            return encodeAsData(json, options: JSONEncoderOptions(pretty: pretty))
        }
    }
    
    extension JSON {
        /// Converts a JSON-compatible Foundation object into a `JSON` value.
        /// - Throws: `JSONFoundationError` if the object is not JSON-compatible.
        public init(ns: Any) throws {
            let object = ns as AnyObject
            if object === kCFBooleanTrue {
                self = .bool(true)
                return
            } else if object === kCFBooleanFalse {
                self = .bool(false)
                return
            }
            switch object {
            case is NSNull:
                self = .null
            case let n as NSNumber:
                let typeChar: UnicodeScalar
                let objCType = n.objCType
                if objCType[0] == 0 || objCType[1] != 0 {
                    typeChar = "?"
                } else {
                    typeChar = UnicodeScalar(UInt8(bitPattern: objCType[0]))
                }
                switch typeChar {
                case "c", "i", "s", "l", "q", "C", "I", "S", "L", "B":
                    self = .int64(n.int64Value)
                case "Q": // unsigned long long
                    let val = n.uint64Value
                    if val > UInt64(Int64.max) {
                        fallthrough
                    }
                    self = .int64(Int64(val))
                default:
                    self = .double(n.doubleValue)
                }
            case let s as String:
                self = .string(s)
            case let dict as NSDictionary:
                var obj: [String: JSON] = Dictionary(minimumCapacity: dict.count)
                for (key, value) in dict {
                    guard let key = key as? String else { throw JSONFoundationError.nonStringKey }
                    obj[key] = try JSON(ns: value)
                }
                self = .object(JSONObject(obj))
            case let array as NSArray:
                var ary: JSONArray = []
                ary.reserveCapacity(array.count)
                for elt in array {
                    ary.append(try JSON(ns: elt))
                }
                self = .array(ary)
            default:
                throw JSONFoundationError.incompatibleType
            }
        }
        
        /// Returns the JSON as a JSON-compatible Foundation object.
        public var ns: Any {
            switch self {
            case .null: return NSNull()
            case .bool(let b): return NSNumber(value: b)
            case .string(let s): return s
            case .int64(let i): return NSNumber(value: i)
            case .double(let d): return d
            case .object(let obj): return obj.ns
            case .array(let ary):
                return ary.map({$0.ns})
            }
        }
        
        /// Returns the JSON as a JSON-compatible Foundation object, discarding any nulls.
        public var nsNoNull: Any? {
            switch self {
            case .null: return nil
            case .bool(let b): return NSNumber(value: b)
            case .string(let s): return s
            case .int64(let i): return NSNumber(value: i)
            case .double(let d): return d
            case .object(let obj): return obj.nsNoNull
            case .array(let ary):
                return ary.flatMap({$0.nsNoNull})
            }
        }
    }
    
    extension JSONObject {
        /// Returns the JSON as a JSON-compatible dictionary.
        public var ns: [AnyHashable: Any] {
            var dict: [AnyHashable: Any] = Dictionary(minimumCapacity: count)
            for (key, value) in self {
                dict[key] = value.ns
            }
            return dict
        }
        
        /// Returns the JSON as a JSON-compatible dictionary, discarding any nulls.
        public var nsNoNull: [AnyHashable: Any] {
            var dict: [AnyHashable: Any] = Dictionary(minimumCapacity: count)
            for (key, value) in self {
                if let value = value.nsNoNull {
                    dict[key] = value
                }
            }
            return dict
        }
    }
    
    // MARK: - Errors
    
    /// An error that is thrown when converting from `AnyObject` to `JSON`.
    /// - SeeAlso: `JSON.init(ns:)`
    public enum JSONFoundationError: Error {
        /// Thrown when a non-JSON-compatible type is found.
        case incompatibleType
        /// Thrown when a dictionary has a key that is not a string.
        case nonStringKey
    }
    
    extension JSONError: LocalizedError {
        public var errorDescription: String? {
            return String(describing: self)
        }
    }
    
    extension JSONParserError: CustomNSError {
        public static let errorDomain: String = "PMJSON.JSONParserError"
        
        public var errorCode: Int {
            return code.rawValue
        }
        
        public var errorUserInfo: [String: Any] {
            return [NSLocalizedDescriptionKey: String(describing: self)]
        }
    }
    
    extension JSONDecoderError: LocalizedError {
        public var failureReason: String? {
            switch self {
            case .streamEnded: return "The JSON event stream ended."
            case .unexpectedToken: return "The JSON event stream contained more than one top-level value."
            case .exceededDepthLimit: return "The JSON event stream exceeded the nesting depth limit."
            }
        }
    }
    
    // MARK: -
    
    private struct UTF8Decoder: Sequence {
        init(data: Data) {
            self.data = data as NSData
        }
        
        func makeIterator() -> Iterator {
            return Iterator(data: data)
        }
        
        private let data: NSData
        
        fileprivate struct Iterator: IteratorProtocol {
            init(data: NSData) {
                // NB: We use NSData instead of using Data's iterator because it's significantly faster as of Xcode 8b3
                self.data = data
                let ptr = UnsafeBufferPointer(start: data.bytes.assumingMemoryBound(to: UInt8.self), count: data.length)
                iter = ptr.makeIterator()
                if ptr.count >= 3 && ptr[0] == 0xEF && ptr[1] == 0xBB && ptr[2] == 0xBF {
                    // UTF-8 BOM detected. Skip it.
                    _ = iter.next(); _ = iter.next(); _ = iter.next()
                }
                utf8 = UTF8()
            }
            
            mutating func next() -> UnicodeScalar? {
                switch utf8.decode(&iter) {
                case .scalarValue(let scalar): return scalar
                case .error: return "\u{FFFD}"
                case .emptyInput: return nil
                }
            }
            
            private let data: NSData
            private var iter: UnsafeBufferPointerIterator<UInt8>
            private var utf8: UTF8
        }
    }
    
    private struct UTF16Decoder: Sequence {
        /// Checks if the data has a UTF-16 BOM, and if so, returns the endianness.
        static func checkBOM(for data: Data) -> Endian? {
            guard data.count >= 2 else { return nil }
            switch (data[0], data[1]) {
            case (0xFE, 0xFF): return .big
            case (0xFF, 0xFE): return .little
            default: return nil
            }
        }
        
        enum Endian {
            case big
            case little
        }
        
        init(data: Data, endian: Endian) {
            self.data = data as NSData
            self.endian = endian
        }
        
        func makeIterator() -> Iterator {
            return Iterator(data: data, endian: endian)
        }
        
        private let data: NSData
        private let endian: Endian
        
        fileprivate struct Iterator: IteratorProtocol {
            init(data: NSData, endian: Endian) {
                iter = DataIterator(data: data, endian: endian)
                utf16 = UTF16()
            }
            
            mutating func next() -> UnicodeScalar? {
                switch utf16.decode(&iter) {
                case .scalarValue(let scalar): return scalar
                case .error: return "\u{FFFD}"
                case .emptyInput: return nil
                }
            }
            
            private var iter: DataIterator
            private var utf16: UTF16
        }
        
        private struct DataIterator: IteratorProtocol {
            init(data: NSData, endian: Endian) {
                // NB: We use NSData instead of using Data's iterator because it's significantly faster as of Xcode 8b3
                self.data = data
                self.endian = endian
                let ptr = UnsafeBufferPointer(start: data.bytes.assumingMemoryBound(to: UInt16.self), count: data.length / 2)
                iter = ptr.makeIterator()
                if !ptr.isEmpty && (ptr[0] == 0xFEFF || ptr[0] == 0xFFFE) {
                    // Skip the BOM
                    _ = iter.next()
                }
                trailingFFFD = data.length % 2 == 1
            }
            
            mutating func next() -> UInt16? {
                switch (iter.next(), endian) {
                case (let x?, .big): return UInt16(bigEndian: x)
                case (let x?, .little): return UInt16(littleEndian: x)
                case (nil, _) where trailingFFFD:
                    trailingFFFD = false
                    return 0xFFFD
                default: return nil
                }
            }
            
            private let data: NSData
            private let endian: Endian
            private var iter: UnsafeBufferPointerIterator<UInt16>
            private var trailingFFFD: Bool
        }
    }
    
#endif // os(iOS) || os(OSX) || os(watchOS) || os(tvOS)
