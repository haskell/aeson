//
//  JSONEncodable.swift
//  Freddy
//
//  Created by Matthew Mathias on 1/4/16.
//  Copyright © 2016 Big Nerd Ranch. All rights reserved.
//

import Foundation

/// A protocol to facilitate encoding and decoding of `JSON`.
public protocol JSONEncodable {
    /// Converts an instance of a conforming type to `JSON`.
    /// - returns: An instance of `JSON`.
    /// - Note: If conforming to `JSONEncodable` with a custom type of your own, you should return an instance of 
    /// `JSON.dictionary`.
    func toJSON() -> JSON
}

extension Array where Element: JSONEncodable {
    /// Converts an instance of `Array` whose elements conform to `JSONEncodable` to `JSON`.
    /// - returns: An instance of `JSON` where the enum case is `.array`.
    public func toJSON() -> JSON {
        let arrayOfJSON = self.map { $0.toJSON() }
        return .array(arrayOfJSON)
    }
}

extension Dictionary where Value: JSONEncodable {
    /// Converts an instance of `Dictionary` whose values conform to `JSONEncodable` to `JSON`.  The keys in the resulting
    /// `JSON.dictionary` will be of type `String`.
    /// - returns: An instance of `JSON` where the enum case is `.dictionary`.
    public func toJSON() -> JSON {
        var jsonDictionary = [String: JSON]()
        
        for (k, v) in self {
            let key = String(describing: k)
            jsonDictionary[key] = v.toJSON()
        }
        
        return .dictionary(jsonDictionary)
    }
}

extension Int: JSONEncodable {
    /// Converts an instance of a conforming type to `JSON`.
    /// - returns: An instance of `JSON` where the enum case is `.int`.
    public func toJSON() -> JSON {
        return .int(self)
    }
}

extension Double: JSONEncodable {
    /// Converts an instance of a conforming type to `JSON`.
    /// - returns: An instance of `JSON` where the enum case is `.double`.
    public func toJSON() -> JSON {
        return .double(self)
    }
}

extension String: JSONEncodable {
    /// Converts an instance of a conforming type to `JSON`.
    /// - returns: An instance of `JSON` where the enum case is `.string`.
    public func toJSON() -> JSON {
        return .string(self)
    }
}

extension Bool: JSONEncodable {
    /// Converts an instance of a conforming type to `JSON`.
    /// - returns: An instance of `JSON` where the enum case is `.bool`.
    public func toJSON() -> JSON {
        return .bool(self)
    }
}

extension RawRepresentable where RawValue: JSONEncodable {
    /// Converts an instance of a conforming type to `JSON`.
    /// - returns: An instance of `JSON` where the enum case is whatever the underlying `RawValue` converts to.
    public func toJSON() -> JSON {
        return rawValue.toJSON()
    }
}
