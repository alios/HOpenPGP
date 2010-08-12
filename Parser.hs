{-# OPTIONS -fglasgow-exts #-}


module GisServer.Nmea0183.Parser () where

import Data.Bits
import Data.Word
import Data.Char
import Data.ByteString.Internal(c2w, w2c)
import Data.Generics(Data)
import Data.Typeable    
import Data.Binary
import Data.Binary.Get
import Control.Applicative

isNmeaChar :: Word8 -> Bool
isNmeaChar c = (c .&. 0x80) == 0x00

startChar = c2w '$'
isStartChar = (==) startChar


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

getTransmittableChar :: Get Word8
getTransmittableChar = 
    do c <- getWord8
       if (isTransmittableChar c) then 
           return c else
           fail $ show c ++ " is not a transmittable char."
                  

addrFieldChars = [ c | c <- validChars
                 , (isUpper $ w2c c) || (isDigit $ w2c c)]
isAddrFieldChar c = elem c addrFieldChars

data AddressField = 
    AAF { 
      talkerId :: String,
      format :: String 
    } | QAF
    {
      
    } 
    deriving (Read, Show, Eq, Data, Typeable)
                  


--x = lookAheadM . optional   

{-
skipUntil :: Get a -> Get a
skipUntil p = do
  r <- lookAheadM p
  case r of
    Just c  -> return c
    Nothing -> skipUntil p

-}