// Copyright (C) 2016 Big Nerd Ranch, Inc. Licensed under the MIT license WITHOUT ANY WARRANTY.

import Foundation

// MARK: - Serialize JSON

extension JSON {

    /// Attempt to serialize `JSON` into an `Data`.
    /// - returns: A byte-stream containing the `JSON` ready for wire transfer.
    /// - throws: Errors that arise from `JSONSerialization`.
    /// - see: Foundation.JSONSerialization
    public func serialize() throws -> Data {
        return try JSONSerialization.data(withJSONObject: toJSONSerializationValue(), options: [])
    }
    
    /// Attempt to serialize `JSON` into a `String`.
    /// - returns: A `String` containing the `JSON`.
    /// - throws: A `JSON.Error.StringSerializationError` or errors that arise from `JSONSerialization`.
    /// - see: Foundation.JSONSerialization
    public func serializeString() throws -> String {
        let data = try self.serialize()
        guard let json = String(data: data, encoding: String.Encoding.utf8) else {
            throw Error.stringSerializationError
        }
        return json
    }

    /// A function to help with the serialization of `JSON`.
    /// - returns: An `Any` suitable for `JSONSerialization`'s use.
    private func toJSONSerializationValue() -> Any {
        switch self {
        case .array(let jsonArray):
            return jsonArray.map { $0.toJSONSerializationValue() }
        case .dictionary(let jsonDictionary):
            var cocoaDictionary = Swift.Dictionary<Swift.String, Any>(minimumCapacity: jsonDictionary.count)
            for (key, json) in jsonDictionary {
                cocoaDictionary[key] = json.toJSONSerializationValue()
            }
            return cocoaDictionary
        case .string(let str):
            return str
        case .double(let num):
            return NSNumber(value: num)
        case .int(let int):
            return NSNumber(value: int)
        case .bool(let b):
            return NSNumber(value: b)
        case .null:
            return NSNull()
        }

    }
}
