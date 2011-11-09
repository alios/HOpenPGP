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
import Data.Time (UTCTime(..), secondsToDiffTime, Day (..))
import System.Time (ClockTime(..))

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Data Type Tests" [
                testProperty "MPI binary test" prop_MPIBinary,
                testProperty "KeyID binary test" prop_KeyIDParser,
                testGroup "Length Encoding Decoding" [
                  testProperty "1,2,5 length binary test" prop_125LengthBinary
                ],
                testProperty "UTCTime binary test" prop_UTCTimeBinary,
                testProperty "StringToKeySpecifier binary test" prop_StringToKeySpecifierBinary,
                testProperty "SignatureType binary test" prop_SignatureTypeBinary,
                testProperty "PublickeyAlgorithm binary test" prop_PublicKeyAlgorithmBinary
            ],
        testGroup "Packet Binary Tests" [
                testProperty "PEKSKP binary test" prop_PEKSKPBinary,
                testProperty "Signature v3 test" prop_Signature3Binary,
                testProperty "Publice Key v4 test" prop_PublicKey4Binary
            ]
    ]

--
-- helpers 
--
rPut :: Binary t => t -> ByteString
rPut = runPut . put

rGet :: Binary t => ByteString -> t
rGet bs = runGet get bs

rPutGet :: Binary t => t -> t
rPutGet = rGet . rPut

putGetTest :: (Binary a, Eq a) => String -> a -> Property
putGetTest l p = label l $ p == rPutGet p

--
-- properties / testables
--

prop_MPIBinary :: MPI -> Property
prop_MPIBinary = putGetTest "MPI binary test"
  
prop_KeyIDParser :: KeyID -> Property
prop_KeyIDParser kid =     
  let ekid = convert $ rPut kid
      rdkid = A.maybeResult $ A.parse parseKeyID ekid
      dkid = case rdkid of
        Nothing -> error "unable to parse KeyID"
        Just a -> a
  in label "KeyID parser test" (kid == dkid)
     
prop_LengthBinary :: Word32 -> Property
prop_LengthBinary i =
  let p = runPut $ put125Length i
      g = parserToGet . bodyLenParser $ if (i <= 191) then OneOctedLength
          else if (i <= 8383) then TwoOctedLength
               else FiveOctedLength
  in label "length binary test" $ i == (runGet g p)
     
prop_125LengthBinary :: Word32 -> Property
prop_125LengthBinary i' = 
  let i = i'
      putI = runPut $ put125Length i
      get125Length = parserToGet parse125Length
      getI = runGet get125Length putI
  in label "1,2,5 octet length binary test" $ (i == getI)
prop_UTCTimeBinary :: UTCTime -> Property
prop_UTCTimeBinary = putGetTest "UTCTime binary test"

prop_StringToKeySpecifierBinary :: StringToKeySpecifier -> Property
prop_StringToKeySpecifierBinary = putGetTest "StringToKeySpecifier binary test"
  
prop_SignatureTypeBinary :: SignatureType -> Property    
prop_SignatureTypeBinary = putGetTest "SignatureType binary test"
  
prop_PublicKeyAlgorithmBinary :: PublicKeyAlgorithm -> Property
prop_PublicKeyAlgorithmBinary = putGetTest "PublicKeyAlgorithm binary test"
  
prop_PEKSKPBinary :: PacketState PEKSKP -> Property
prop_PEKSKPBinary = putGetTest "PacketState PEKSKP binary test"
    
prop_Signature3Binary :: PacketState Signature3 -> Property
prop_Signature3Binary = putGetTest "PacketState Signature3 binary test"
    
prop_PublicKey4Binary :: PacketState PublicKey4 -> Property
prop_PublicKey4Binary = putGetTest "PacketState PublicKey4 binary test"

--
-- Arbitrary instances
--

instance Arbitrary SignatureType where
  arbitrary = elements $ map fst signatureTypeCoding

instance Arbitrary HashAlgorithm where
  arbitrary = elements [ MD5,SHA1,RIPEMD160,SHA256,SHA384,SHA512,SHA224 ]

instance Arbitrary MPI where
  arbitrary = fmap (MPI . BS.pack) arbitrary
    
instance Arbitrary UTCTime where
  arbitrary = do
  i <- arbitrary :: Gen Word32
  return $ convert $ TOD (abs $ convert i) 0

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

instance Arbitrary PublicKeyAlgorithm where
  arbitrary = elements $ map fst publicKeyAlgorithmCoding  

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
    
instance Arbitrary (PacketState Signature3) where
  arbitrary = do
    ty <- arbitrary
    t <- arbitrary
    k <- arbitrary
    a <- elements [ RSAEncryptOrSign, RSASignOnly, DSA ]
    h <- arbitrary
    f <- arbitrary
    d <- case a of 
      RSAEncryptOrSign -> fmap Left arbitrary
      RSASignOnly -> fmap Left arbitrary
      DSA -> fmap Right arbitrary
    return $ MkSignaturePacket3 ty t k a h f d

instance Arbitrary (PacketState PublicKey4) where
  arbitrary = do
    t <- arbitrary
    a <- arbitrary
    m1 <- arbitrary
    m2 <- arbitrary
    m3 <- arbitrary
    m4 <- arbitrary
    return $ MkPublicKeyPacket4 t a $ case a of 
      RSAEncryptOrSign -> RSAPKMaterial m1 m2
      RSASignOnly -> RSAPKMaterial m1 m2
      RSAEncryptOnly -> RSAPKMaterial m1 m2      
      DSA -> DSAPKMaterial m1 m2 m3 m4
      ElgamalEncryptOnly -> ElgamalPKMaterial m1 m2 m3

