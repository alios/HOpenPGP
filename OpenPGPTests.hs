{-# LANGUAGE FlexibleInstances #-}

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
  chk prop_StringToKeySpecifierBinary
  chk prop_SignatureTypeBinary
  chk prop_PublicKeyAlgorithmBinary
  chk prop_PEKSKPBinary
  return ()
  where 
    chk :: Testable prop => prop -> IO ()
    chk = quickCheck
--    chk = verboseCheck
    
    
instance Arbitrary HashAlgorithm where
  arbitrary = elements [ MD5,SHA1,RIPEMD160,SHA256,SHA384,SHA512,SHA224 ]
        
rPut :: Binary t => t -> ByteString
rPut = runPut . put

rGet :: Binary t => ByteString -> t
rGet bs = runGet get bs

rPutGet :: Binary t => t -> t
rPutGet = rGet . rPut

instance Arbitrary MPI where
  arbitrary = fmap (MPI . BS.pack) arbitrary
    
    

prop_MPIBinary :: [Word8] -> Property
prop_MPIBinary bs =
  let mpi = (MPI . BS.pack) bs
  in label "MPI binary test" (mpi == rPutGet mpi)
  

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
  in label "UTCTime binary test" $ utc == rPutGet utc


instance Arbitrary StringToKeySpecifier where
  arbitrary = do
    n <- choose (0, 2) :: Gen Int
    a <- arbitrary
    s <- arbitrary
    i <- arbitrary
    return $ case n of
      0 -> SimpleS2K a
      1 -> SaltedS2K a s
      2 -> IteratedAndSaltedS2K a s i

prop_StringToKeySpecifierBinary :: StringToKeySpecifier -> Property
prop_StringToKeySpecifierBinary s2k =  
  label "StringToKeySpecifier binary test" $ s2k == rPutGet s2k
  
instance Arbitrary SignatureType where
  arbitrary = elements $ map fst signatureTypeCoding

prop_SignatureTypeBinary :: SignatureType -> Property    
prop_SignatureTypeBinary s = 
  label "SignatureType binary test" $ s == rPutGet s
  
instance Arbitrary PublicKeyAlgorithm where
  arbitrary = elements $ map fst publicKeyAlgorithmCoding  

prop_PublicKeyAlgorithmBinary :: PublicKeyAlgorithm -> Property
prop_PublicKeyAlgorithmBinary a =
  label "PublicKeyAlgorithm binary test" $ a == rPutGet a
  
instance Arbitrary (PacketState PEKSKP) where
  arbitrary = do
    keyid' <- arbitrary
    let keyid = if (keyid' == 0) then Nothing else Just keyid'
    a <- arbitrary
    b <- arbitrary
    n <- choose (0, 2) :: Gen Int
    return $ case n of 
             0 -> MkPEKSKP keyid RSAEncryptOrSign (Left a)
             1 -> MkPEKSKP keyid RSAEncryptOrSign (Left a)
             2 -> MkPEKSKP keyid ElgamalEncryptOnly (Right (a,b))
    
prop_PEKSKPBinary :: PacketState PEKSKP -> Property
prop_PEKSKPBinary p = 
    label "PacketState PEKSKP binary test" $ p == rPutGet p
    

