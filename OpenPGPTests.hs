
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
  chk prop_UTCTimeParser
  return ()
  where 
    chk :: Testable prop => prop -> IO ()
    chk = quickCheck
 -- chk = verboseCheck
    
rPut :: (Binary t) => t -> ByteString
rPut = runPut . put

rGet :: Binary t => ByteString -> t
rGet bs = runGet get bs

prop_MPIBinary :: [Word8] -> Property
prop_MPIBinary bs =
  let mpi = (MPI . BS.pack) bs
      empi = rPut mpi
      dmpi = rGet empi
  in label "binary encoding / decoding of MPI" (mpi == dmpi)
  

prop_KeyIDParser :: KeyID -> Property
prop_KeyIDParser kid =     
  let ekid = convert $ rPut kid
      rdkid = A.maybeResult $ A.parse parseKeyID ekid
      dkid = case rdkid of
        Nothing -> error "unable to parse KeyID"
        Just a -> a
  in label "KeyID parser test" (kid == dkid)
     
prop_UTCTimeParser :: Integer -> Property
prop_UTCTimeParser i = 
  let utc :: UTCTime
      utc = convert $ TOD (abs i) 0
  in label "UTCTime parser test" $ 
     case (A.maybeResult $ A.parse parseTime (convert $ rPut utc)) of
       Nothing -> error "unable to parse Time"
       Just utc' -> utc == utc' 

{-
instance Arbitrary UTCTime where
  arbitrary = do
    offset <- choose (0, 20000) :: Gen Float
    return . fromMJD' $ offset + fromRational startOfTimeMJD
-}