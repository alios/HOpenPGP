module Nmea0183 ( isNmeaChar 
                , startChar  
                , isStartChar 
                , fieldSep
                , isFieldSep  
                , checksumSep
                , isChecksumSep
                , reservedChars 
                , isReservedChar 
                , validChars
                , isValidChar
                , transmittableChars
                , isTransmittableChar
                , undefChars
                , isUndefChar
                ) where

import Data.Word
import Data.Bits
import Data.Char
import Data.ByteString.Internal(c2w, w2c)

isNmeaChar :: Word8 -> Bool
isNmeaChar c = (c .&. 0x80) == 0x00

startChar = c2w '$'
isStartChar = (==) startChar

checksumSep = c2w '*'
isChecksumSep = (==) checksumSep

fieldSep = c2w ','
isFieldSep = (==) fieldSep

reservedChars :: [ Word8 ]
reservedChars = map c2w "$*,!\\^~" ++ [ 0x0d, 0x0a ]
isReservedChar c = elem c reservedChars

validChars = [ c | c <- [0x20 .. 0x7e], not $ isReservedChar c]
isValidChar c = elem c validChars

-- TODO: add test case that all valid and Reserved chars are transmittable


undefChars = [ c | c <- [0x00 .. 0xff]
             , isNmeaChar c
             , not $ isReservedChar c
             , not $ isValidChar c]

isUndefChar c = elem c undefChars

transmittableChars = [ c | c <- reservedChars ++ validChars
                     , isNmeaChar c]
isTransmittableChar c = elem c transmittableChars

addrFieldChars = [ c | c <- validChars
                 , (isUpper $ w2c c) || (isDigit $ w2c c)]
isAddrFieldChar c = elem c addrFieldChars

