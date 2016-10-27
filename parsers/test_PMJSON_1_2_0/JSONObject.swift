//
//  JSONObject.swift
//  PMJSON
//
//  Created by Kevin Ballard on 11/10/15.
//  Copyright © 2016 Postmates.
//
//  Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
//  http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
//  <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
//  option. This file may not be copied, modified, or distributed
//  except according to those terms.
//

/// A collection of key-value pairs that maps `String` to `JSON`.
///
/// This collection abstracts away the underlying representation and allows for JSON-specific
/// methods to be added.
public struct JSONObject {
    /// Creates an empty object.
    public init() {
        dictionary = [:]
    }
    
    /// Creates an object from a sequence of `(String,JSON)` pairs.
    public init<S: Sequence>(_ seq: S) where S.Iterator.Element == (String,JSON) {
        // optimize for the case where the sequence doesn't contain duplicate keys
        dictionary = Dictionary(minimumCapacity: seq.underestimatedCount)
        for (key,value) in seq {
            dictionary[key] = value
        }
    }
    
    /// The JSON object represented as a `[String: JSON]`.
    public fileprivate(set) var dictionary: [String: JSON]
    
    public subscript(key: String) -> JSON? {
        @inline(__always) get {
            return dictionary[key]
        }
        @inline(__always) set {
            dictionary[key] = newValue
        }
    }
}

extension JSONObject: Collection {
    /// The position of the first element in a non-empty object.
    ///
    /// Identical to `endIndex` in an empty object.
    public var startIndex: Index {
        return Index(dictionary.startIndex)
    }
    
    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always reachable from `startIndex`
    /// by zero or more applications of `successor()`.
    public var endIndex: Index {
        return Index(dictionary.endIndex)
    }
    
    /// Returns `true` iff `self` is empty.
    public var isEmpty: Bool {
        return dictionary.isEmpty
    }
    
    /// The number of entries in the object.
    public var count: Int {
        return dictionary.count
    }
    
    public func makeIterator() -> Iterator {
        return Iterator(dictionary.makeIterator())
    }
    
    public subscript(position: Index) -> (String,JSON) {
        return dictionary[position.base]
    }
    
    public func index(after i: Index) -> Index {
        return Index(dictionary.index(after: i.base))
    }
    
    public func formIndex(after i: inout Index) {
        dictionary.formIndex(after: &i.base)
    }
    
    /// Represents a position in a `JSONObject`.
    public struct Index: Comparable {
        fileprivate var base: Dictionary<String,JSON>.Index
        
        fileprivate init(_ base: Dictionary<String,JSON>.Index) {
            self.base = base
        }
        
        public static func ==(lhs: JSONObject.Index, rhs: JSONObject.Index) -> Bool {
            return lhs.base == rhs.base
        }
        
        public static func <(lhs: JSONObject.Index, rhs: JSONObject.Index) -> Bool {
            return lhs.base < rhs.base
        }
    }
    
    public struct Iterator: IteratorProtocol {
        private var base: Dictionary<String,JSON>.Iterator
        
        fileprivate init(_ base: Dictionary<String,JSON>.Iterator) {
            self.base = base
        }
        
        /// Advance to the next element and return it, or `nil` if no next element exists.
        ///
        /// - Requires: No preceding call to `self.next()` has returned `nil`.
        public mutating func next() -> (String,JSON)? {
            return base.next()
        }
    }
}

extension JSONObject {
    /// A collection containing just the keys of `self`.
    ///
    /// Keys appear in the same order as they occur as the `.0` member of key-value pairs in `self`.
    /// Each key in the result has a unique value.
    public var keys: LazyMapCollection<JSONObject, String> {
        return lazy.map({ $0.0 })
    }
    
    /// A collection containing just the values of `self`.
    ///
    /// Values appear in the same order as they occur as the `.1` member of key-value pairs in `self`.
    public var values: LazyMapCollection<JSONObject, JSON> {
        return lazy.map({ $0.1 })
    }
    
    /// Returns the `Index` for the given key, or `nil` if the key is not present in the object.
    public func indexForKey(_ key: String) -> Index? {
        return dictionary.index(forKey: key).map(Index.init)
    }
    
    /// Update the value stored in the object for the given key, or, if the key does not exist,
    /// add a new key-value pair to the object.
    ///
    /// Returns the value that was replaced, or `nil` if a new key-value pair was added.
    public mutating func updateValue(_ value: JSON, forKey key: String) -> JSON? {
        return dictionary.updateValue(value, forKey: key)
    }
    
    /// Remove the key-value pair at `index`.
    ///
    /// Invalidates all indices with respect to `self`.
    public mutating func removeAtIndex(_ index: Index) -> (String,JSON)? {
        return dictionary.remove(at: index.base)
    }
    
    /// Remove a given key and the associated value from the object.
    /// Returns the value that was removed, or `nil` if the key was not present in the object.
    public mutating func removeValueForKey(_ key: String) -> JSON? {
        return dictionary.removeValue(forKey: key)
    }
    
    /// Remove all elements.
    ///
    /// Invalidates all indices with respect to `self`.
    public mutating func removeAll() {
        dictionary.removeAll()
    }
    
    /// If `!self.isEmpty`, return the first key-value pair in the sequence of elements, otherwise return `nil`.
    public mutating func popFirst() -> (String,JSON)? {
        return dictionary.popFirst()
    }
}

extension JSONObject: ExpressibleByDictionaryLiteral {
    /// Creates an object from a dictionary.
    public init(_ dictionary: [String: JSON]) {
        self.dictionary = dictionary
    }
    
    /// Creates an object initialized with `elements`.
    public init(dictionaryLiteral elements: (String, JSON)...) {
        self.init(elements)
    }
}

extension JSONObject: TextOutputStreamable, CustomStringConvertible, CustomDebugStringConvertible {
    public func write<Target : TextOutputStream>(to target: inout Target) {
        JSON.encode(JSON(self), to: &target)
    }
    
    public var description: String {
        return JSON.encodeAsString(JSON(self))
    }
    
    public var debugDescription: String {
        let desc = JSON.encodeAsString(JSON(self))
        return "JSONObject(\(desc))"
    }
}

extension JSONObject: Equatable {
    public static func ==(lhs: JSONObject, rhs: JSONObject) -> Bool {
        return lhs.dictionary == rhs.dictionary
    }
}

extension JSONObject: CustomReflectable {
    public var customMirror: Mirror {
        let children: LazyMapCollection<Dictionary<String, JSON>, Mirror.Child> = dictionary.lazy.map({ ($0,$1) })
        return Mirror(self, children: children, displayStyle: .dictionary)
    }
}
