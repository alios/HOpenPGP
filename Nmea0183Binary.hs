module Nmea0183Binary where
import Data.Word
import Data.Char(toUpper)
import Data.Int (Int64)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Encoding (encodeLazyByteString, decodeLazyByteString)
import Data.Encoding.ASCII
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Numeric
import qualified Text.ParserCombinators.ReadP as P
type Checksum = Word8
type Tag = (B.ByteString, B.ByteString)

data NmeaRawSentence =
  NmeaRawSentence {
    rawSentenceTag :: Tag,
    rawSentenceFields :: [B.ByteString], 
    rawSentenceChecksum :: Maybe Checksum
    } deriving (Show, Eq)
  
instance Binary NmeaRawSentence where
  get = getSentence
  put = putSentence

putSentence :: NmeaRawSentence -> Put
putSentence (NmeaRawSentence (talkerId ,typeCode) fs cs) = do
  let tag = B.concat [talkerId,typeCode]
  let fieldArea = B.intercalate (B.pack [fieldDelim]) (tag : fs) 
  let ccs = crcBS fieldArea
      
  case (cs) of
    Nothing -> putSentence' fieldArea ccs
    (Just cs'') -> 
      if (ccs /= cs'') then 
        fail $ "calculated checksum " ++ show ccs ++ 
        " does not match given checksum " ++ show cs else 
        putSentence' fieldArea cs''  
  flush

putSentence' fieldArea cs = do
  putWord8 startOfSentenceDelim
  putLazyByteString fieldArea
  putWord8 checksumFieldDelim
  putLazyByteString $ encodeLazyByteString ASCII $ map toUpper $ showHex cs ""
  putWord8 cr
  putWord8 lf

getSentence :: Get NmeaRawSentence
getSentence = do
  getNmeaChar startOfSentenceDelim
  tag@(talkerId ,typeCode) <- getTag
  fieldArea <- getWhile (\c -> (c /= checksumFieldDelim) && (c /= cr))
  let fs = runGet getFields fieldArea
  let ccs = crcBS $ B.concat [talkerId,typeCode,fieldArea]
  nxt <- getNmeaByte
  cs  <- if (nxt == checksumFieldDelim) then
           do cs' <- fmap B.pack $ multi 2 getHexDigit  
              return $ Just $ readHex' $ decodeLazyByteString ASCII $ cs' else
           return Nothing
  cr' <- getWord8
  if (cr' /= cr) then fail $ "expected CR but got: " ++ show cr' else do
    lf' <- getWord8
    if (lf' /= lf) then fail $ "expected LF but got: " ++ show lf' else  
      case (cs) of
        (Just cs') -> if (cs' == ccs) then 
                        return $ NmeaRawSentence tag fs cs else
                        fail $ "calculated checksum " ++ show ccs ++ 
                        " does not match read checksum " ++ show cs' 
        (Nothing) -> return $ NmeaRawSentence tag fs $ Just ccs

-- TODO: warning if exceeds maximum field length 82 or maxField Count
    
    
    
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
  talkerId <- fmap B.pack $ multi 2 getValidNmeaByte
  typeCode <- fmap B.pack $ multi 3 getValidNmeaByte
  return (talkerId, typeCode)
  

getFields :: Get [B.ByteString]
getFields = do
  e <- isEmpty
  if (e) then return [] else do 
      f <- getField
      fs <- getFields
      return (f : fs)
    
getField :: Get B.ByteString
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
  
  let b = bs `B.index` n
  if (v b) then
    return b else
    fail $ "lookAheadN: read unexpected char " ++ show b ++
    " in postion " ++ show (p + n)
    
getWhile :: (Word8 -> Bool) -> Get B.ByteString
getWhile p = 
  let getWhile' :: Int64 -> (Word8 -> Bool) -> Get B.ByteString
      getWhile' n p = do
        e <- isEmpty
        if (e) then return B.empty else do
          b <- lookAheadN n isNmeaByte
          if (p b) then
            getWhile' (succ n) p else
            getLazyByteString n
  in getWhile' 0 p
     
crcBS :: B.ByteString -> Word8
crcBS bs = foldl xor 0 (B.unpack bs)

readHex' x = 
  let r = readHex x
      (n, _) = r !! 0 
  in n
  
  
prop_encodeDecode :: NmeaRawSentence -> Bool
prop_encodeDecode s =
  let s' = encode s
  in s == (decode s')
     
t1 = "$GPGSA,A,3,04,05,,09,12,24,2.5,1.3,2.1\r\n"
t1' = encodeLazyByteString ASCII t1
t2 :: NmeaRawSentence
t2 = decode t1'
t3 = encode t2

     
sentences = map (encodeLazyByteString ASCII)
  [ "$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47\r\n" 
  , "$GPGSA,A,3,04,05,,09,12,,,24,,,,,2.5,1.3,2.1*39\r\n"
  , "$GPGSV,2,1,08,01,40,083,46,02,17,308,41,12,07,344,39,14,22,228,45*75\r\n"
  ]
   
            
