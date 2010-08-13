module Nmea0183Binary where
import Data.Word
import Data.Int (Int64)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString, index, pack)
import Data.Encoding
import Data.Encoding.ASCII
import System.IO.Encoding

type Checksum = Word8
type Tag = ([Word8], [Word8])

data NmeaRawSentence =
  NmeaRawSentence {
    rawSentenceTag :: Tag,
    rawSentenceFields :: [ByteString], 
    rawSentenceChecksum :: Maybe Checksum
    }
  
instance Binary NmeaRawSentence where
  get = getSentence 3

getSentence :: Int -> Get NmeaRawSentence
getSentence n = do
  getNmeaChar startOfSentenceDelim
  tag <- getTag
  fieldArea <- getWhile (\c -> (c /= checksumFieldDelim) && (c /= cr))
  let fs = runGet (multi n getField) fieldArea
  nxt <- getNmeaByte
  cs  <- if (nxt == checksumFieldDelim) then
           do cs' <- fmap pack $ multi 2 getHexDigit 
              return $ Just $ read $ decodeLazyByteString ASCII $ cs' else
           return Nothing
  getNmeaChar cr
  getNmeaChar lf
  return $ NmeaRawSentence tag fs cs


--
-- Character binary definitions
--

startOfSentenceDelim :: Word8
startOfSentenceDelim = 36

checksumFieldDelim :: Word8
checksumFieldDelim = 42

cr = 0x0d -- <CR>
lf = 0x0a -- <LF>
fieldDelim = 0x2c -- ,

isNmeaByte c = (c >= 0x20) && (c <= 0x7e)
nmeaBytes = [c | c <- [0x0 .. 0x7f], isNmeaByte c]

hexDigits :: [Word8]
hexDigits = map (fromIntegral . fromEnum) $ ['0'..'9'] ++ ['A' .. 'F'] 

isHexDigit c = elem c hexDigits 

isValidNmeaByte :: Word8 -> Bool
isValidNmeaByte c =  
   isNmeaByte c && (not $ isReservedByte c)
  
validNmeaBytes = [ c | c <- nmeaBytes, isValidNmeaByte c]
  
isReservedByte :: Word8 -> Bool
isReservedByte c = elem c reservedBytes

reservedBytes 
  = [cr ,lf, startOfSentenceDelim, checksumFieldDelim, fieldDelim] ++ 
    [ 0x21 -- !
    , 0x5c -- \ 
    , 0x5e -- ^
    , 0x73 -- ~
    ]

     
getNmeaByte = do
  b <- getWord8
  if (isNmeaByte b) then
    return b else
    fail $ "expected NMEA byte but read '" ++ show b ++ "'"
        
getHexDigit = do
  b <- getNmeaByte
  if(isHexDigit b) then
    return b else
    fail $ "expected hex digit but read '"++ show b ++ "'"
  
getValidNmeaByte = do
  b <- getNmeaByte
  if(isValidNmeaByte b) then
    return b else
    fail $ "expected valid NMEA byte but read '"++ show b ++ "'"

getNmeaChar c = do
  b <- getNmeaByte
  if (b == c) then 
    return b else
    fail $ "expected byte " ++ show c ++" but read '" ++ show b ++ "'"

getTag :: Get Tag
getTag = do
  talkerId <- multi 2 getValidNmeaByte
  typeCode <- multi 3 getValidNmeaByte
  return (talkerId, typeCode)
  
getField :: Get ByteString
getField = do
  getNmeaChar fieldDelim
  f <- getWhile (\c -> (c /= fieldDelim) && isValidNmeaByte c)
  return f

  
    
--
-- Helpers 
--
multi :: Int -> Get a -> Get [a] 
multi n p = sequence $ replicate n p

lookAheadN :: Int64 -> (Word8 -> Bool) -> Get Word8
lookAheadN n v = do  
  p <- bytesRead
  bs <- getRemainingLazyByteString
  
  let b = bs `index` n
  if (v b) then
    return b else
    fail $ "read unexpected char '"++ show b ++" ' in postion" ++ show (p + n)
    
getWhile :: (Word8 -> Bool) -> Get ByteString
getWhile p = 
  let getWhile' :: Int64 -> (Word8 -> Bool) -> Get ByteString
      getWhile' n p = do
        b <- lookAheadN n isNmeaByte
        if (p b) then
          getWhile' (succ n) p else
          getLazyByteString n
  in getWhile' 0 p