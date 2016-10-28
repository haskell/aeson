//
//  JSONEncodingDetector.swift
//  Freddy
//
//  Created by Robert Edwards on 1/27/16.
//  Copyright © 2016 Big Nerd Ranch. All rights reserved.
//

/// Struct for attempting to detect the Unicode encoding used with the data supplied to the JSONParser
public struct JSONEncodingDetector {

    //// The Unicode encodings looked for during detection
    public enum Encoding {
        //// UTF-8
        case utf8
        //// UTF-16 Little Endian
        case utf16LE
        //// UTF-16 Big Endian
        case utf16BE
        //// UTF-32 Little Endian
        case utf32LE
        //// UTF-32 Big Endian
        case utf32BE
    }

    //// The Unicode encodings supported by JSONParser.swift
    public static let supportedEncodings: [Encoding] = [.utf8]

    typealias ByteStreamPrefixInformation = (encoding: Encoding, byteOrderMarkLength: Int)

    //// Attempts to detect the Unicode encoding used for a given set of data.
    ////
    //// This function initially looks for a Byte Order Mark in the following form:
    ////
    ////     Bytes     | Encoding Form
    //// --------------|----------------
    //// 00 00 FE FF   |	UTF-32, big-endian
    //// FF FE 00 00   |	UTF-32, little-endian
    //// FE FF         |	UTF-16, big-endian
    //// FF FE         |	UTF-16, little-endian
    //// EF BB BF      |	UTF-8
    ////
    //// If a BOM is not found then we detect using the following approach described in
    //// the JSON RFC http://www.ietf.org/rfc/rfc4627.txt:
    ////
    //// Since the first two characters of a JSON text will always be ASCII
    //// characters [RFC0020], it is possible to determine whether an octet
    //// stream is UTF-8, UTF-16 (BE or LE), or UTF-32 (BE or LE) by looking
    //// at the pattern of nulls in the first four octets.
    ////
    //// 00 00 00 xx  UTF-32BE
    //// 00 xx 00 xx  UTF-16BE
    //// xx 00 00 00  UTF-32LE
    //// xx 00 xx 00  UTF-16LE
    //// xx xx xx xx  UTF-8
    ////
    //// - parameter header: The front Slice of data being read and evaluated.
    //// - returns: A tuple containing the detected Unicode encoding and the lenght of the byte order mark.
    static func detectEncoding(_ header: RandomAccessSlice<UnsafeBufferPointer<UInt8>>) -> ByteStreamPrefixInformation {

        guard let prefix = prefixFromHeader(header) else {
            return (.utf8, 0)
        }

        if let prefixInfo = JSONEncodingDetector.encodingFromBOM(prefix) {
            return prefixInfo
        } else {
            switch prefix {
            case(0, 0, 0?, _):
                return (.utf32BE, 0)
            case(_, 0, 0?, 0?):
                return (.utf32LE, 0)
            case (0, _, 0?, _), (0, _, _, _):
                return (.utf16BE, 0)
            case (_, 0, _, 0?), (_, 0, _, _):
                return (.utf16LE, 0)
            default:
                return (.utf8, 0)
            }
        }
    }

    private typealias EncodingBytePrefix = (UInt8, UInt8, UInt8?, UInt8?)

    private static func prefixFromHeader(_ header: RandomAccessSlice<UnsafeBufferPointer<UInt8>>) -> EncodingBytePrefix? {
        if header.count >= 4 {
            return(header[0], header[1], header[2], header[3])
        } else if header.count >= 2 {
            return (header[0], header[1], nil, nil)
        }
        return nil
    }

    private static func encodingFromBOM(_ prefix: EncodingBytePrefix) -> ByteStreamPrefixInformation? {
        switch prefix {
        case(0xFE, 0xFF, _, _):
            return (.utf16BE, 2)
        case(0x00, 0x00, 0xFE?, 0xFF?):
            return (.utf32BE, 4)
        case(0xEF, 0xBB, 0xBF?, _):
            return (.utf8, 3)
        case(0xFF, 0xFE, 0?, 0?):
            return (.utf32LE, 4)
        case(0xFF, 0xFE, _, _):
            return (.utf16LE, 2)
        default:
            return nil
        }
    }
}
