
module Main where

import OpenPGP
import Test.QuickCheck
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Attoparsec as A
import Data.Convertible
import Data.Time (UTCTime(..), picosecondsToDiffTime)
import System.Time (ClockTime(..))

main = do
  chk prop_MPIBinary
  chk prop_KeyIDParser 
  chk prop_UTCTimeBinary
  return ()
  where 
    chk :: Testable prop => prop -> IO ()
    chk = quickCheck
 -- chk = verboseCheck
    
rPut :: Binary t => t -> ByteString
rPut = runPut . put

rGet :: Binary t => ByteString -> t
rGet bs = runGet get bs

rPutGet :: Binary t => t -> t
rPutGet = rGet . rPut

prop_MPIBinary :: [Word8] -> Property
prop_MPIBinary bs =
  let mpi = (MPI . BS.pack) bs
  in label "binary encoding / decoding of MPI" (mpi == rPutGet mpi)
  

prop_KeyIDParser :: KeyID -> Property
prop_KeyIDParser kid =     
  let ekid = convert $ rPut kid
      rdkid = A.maybeResult $ A.parse parseKeyID ekid
      dkid = case rdkid of
        Nothing -> error "unable to parse KeyID"
        Just a -> a
  in label "KeyID parser test" (kid == dkid)
     
prop_UTCTimeBinary :: Integer -> Property
prop_UTCTimeBinary i = 
  let utc :: UTCTime
      utc = convert $ TOD (abs i) 0
  in label "UTCTime parser test" $ utc == rPutGet utc

