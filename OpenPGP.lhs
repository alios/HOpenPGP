
\begin{code}
module OpenPGP where
import Data.Binary
import Data.Bits
import Data.Word
import Data.Tuple
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

\end{code}

3.2.  Multiprecision Integers

   Multiprecision integers (also called MPIs) are unsigned integers used
   to hold large integers such as the ones used in cryptographic
   calculations.

   An MPI consists of two pieces: a two-octet scalar that is the length
   of the MPI in bits followed by a string of octets that contain the
   actual integer.

\begin{code}
newtype MPI = MPI (Word16, ByteString)
            deriving (Show, Eq)
                     
\end{code}

   These octets form a big-endian number; a big-endian number can be
   made into an MPI by prefixing it with the appropriate length.

   Examples:

   (all numbers are in hexadecimal)

   The string of octets [00 01 01] forms an MPI with the value 1.  The
   string [00 09 01 FF] forms an MPI with the value of 511.

   Additional rules:

   The size of an MPI is ((MPI.length + 7) / 8) + 2 octets.

   The length field of an MPI describes the length starting from its
   most significant non-zero bit.  Thus, the MPI [00 02 01] is not
   formed correctly.  It should be [00 01 01].

   Unused bits of an MPI MUST be zero.

   Also note that when an MPI is encrypted, the length refers to the
   plaintext MPI.  It may be ill-formed in its ciphertext.

\begin{code}
toMPI :: Integer -> MPI
toMPI i =
  let x = encode i
      l' = B.length x
      maxL = 65535 
      l = if (l' * 8 <= maxL) then 
            8 * ((fromInteger.toInteger) l')
          else
            error $ show i ++ " has " ++ show (l' * 8) ++ " bits but only " ++ show maxL ++ " are allowed"
  in MPI (l, x)

fromMPI :: MPI -> Integer
fromMPI (MPI (l, bs')) =
  let bs = B.take ((fromInteger.toInteger) l) bs'
  in decode bs
  

prop_mpiA i = i == (fromMPI.toMPI) i
\end{code}

3.3.  Key IDs

   A Key ID is an eight-octet scalar that identifies a key.
   Implementations SHOULD NOT assume that Key IDs are unique.  The
   section "Enhanced Key Formats" below describes how Key IDs are
   formed.


\begin{code}
type KeyID = Word64
\end{code}

3.4.  Text
   Unless otherwise specified, the character set for text is the UTF-8
   [RFC3629] encoding of Unicode [ISO10646].


3.5.  Time Fields

   A time field is an unsigned four-octet number containing the number
   of seconds elapsed since midnight, 1 January 1970 UTC.

3.6.  Keyrings

   A keyring is a collection of one or more keys in a file or database.
   Traditionally, a keyring is simply a sequential list of keys, but may
   be any suitable database.  It is beyond the scope of this standard to
   discuss the details of keyrings or other databases.


3.7.  String-to-Key (S2K) Specifiers

   String-to-key (S2K) specifiers are used to convert passphrase strings
   into symmetric-key encryption/decryption keys.  They are used in two
   places, currently: to encrypt the secret part of private keys in the
   private keyring, and to convert passphrases to encryption keys for
   symmetrically encrypted messages.

3.7.1.  String-to-Key (S2K) Specifier Types

   There are three types of S2K specifiers currently supported, and
   some reserved values:

       ID          S2K Type
       --          --------
       0           Simple S2K
       1           Salted S2K
       2           Reserved value
       3           Iterated and Salted S2K
       100 to 110  Private/Experimental S2K

   These are described in Sections 3.7.1.1 - 3.7.1.3.

\begin{code}
data S2KSpecifierType = SimpleS2K | SaltedS2K | ReservedS2K | IteratedAndSaltedS2K
                      deriving (Read, Show, Enum, Eq)
\end{code}

4.2.  Packet Headers

   The first octet of the packet header is called the "Packet Tag".  It
   determines the format of the header and denotes the packet contents.
   The remainder of the packet header is the length of the packet.

   Note that the most significant bit is the leftmost bit, called bit 7.
   A mask for this bit is 0x80 in hexadecimal.

              +---------------+
         PTag |7 6 5 4 3 2 1 0|
              +---------------+
         Bit 7 -- Always one
         Bit 6 -- New packet format if set

   PGP 2.6.x only uses old format packets.  Thus, software that
   interoperates with those versions of PGP must only use old format
   packets.  If interoperability is not an issue, the new packet format
   is RECOMMENDED.  Note that old format packets have four bits of
   packet tags, and new format packets have six; some features cannot be
   used and still be backward-compatible.

   Also note that packets with a tag greater than or equal to 16 MUST
   use new format packets.  The old format packets can only express tags
   less than or equal to 15.

   Old format packets contain:

         Bits 5-2 -- packet tag
         Bits 1-0 -- length-type

   New format packets contain:

         Bits 5-0 -- packet tag

\begin{code}
  
data PacketHeader =
  NewPacketHeader PacketTag |
  OldPacketHeader PacketTag PacketLength
  
instance Binary PacketHeader where
  put (NewPacketHeader t) =
    case (packetTagToNum t) of
      Nothing -> fail $ "unable to lookup tag " ++ show t
      Just p -> putWord8 $ (p `setBit` 6) `setBit` 7
     
  put (OldPacketHeader t l) =
    if (not $ isValidOldTag t) then
      fail $ "packet is old packet but has invalid tag: " ++ show t
    else
      case (packetTagToNum t) of
        Nothing -> fail $ "unable to lookup tag " ++ show t
        Just p -> 
          case (packetLengthToNum l) of
            Nothing -> fail $ ""
            Just l' -> putWord8 $ ((p `shiftR` 2) .|. l' )`setBit` 7

  get = do
    w <- getWord8
    if (not $ testBit w 7) 
      then fail "unable to parse packet header - bit 7 not set"
      else if (testBit w 6)
           then let t = w .&. 0x3F
                in case (lookupPacketTag t) of
                  Nothing -> fail $ "unknown packet tag " ++ show t
                  Just t' -> return $ NewPacketHeader t'
                  
           else let t = (w `shiftL` 2) .&. 0x0F
                    l = w .&. 0x3;
                in if (not $ isValidOldTagNum t) then 
                     fail $ "invalid tag for old packet " ++ show t
                   else
                     case (lookupPacketTag t) of 
                       Nothing -> fail $ "unknown packet tag " ++ show t
                       Just t'-> case (lookupPacketLength l) of
                         Nothing -> fail $ "unknown packet length " ++ show l
                         Just l' -> return $ OldPacketHeader t' l'
  
\end{code}


 4.2.1.  Old Format Packet Lengths

   The meaning of the length-type in old format packets is:

   0 - The packet has a one-octet length.  The header is 2 octets long.

   1 - The packet has a two-octet length.  The header is 3 octets long.

   2 - The packet has a four-octet length.  The header is 5 octets long.

   3 - The packet is of indeterminate length.  The header is 1 octet
       long, and the implementation must determine how long the packet
       is.  If the packet is in a file, this means that the packet
       extends until the end of the file.  In general, an implementation
       SHOULD NOT use indeterminate-length packets except where the end
       of the data will be clear from the context, and even then it is
       better to use a definite length, or a new format header.  The new
       format headers described below have a mechanism for precisely
       encoding data of indeterminate length.

\begin{code}
  
data PacketLength =
  OneOctedLength |
  TwoOctedLength |
  FourOctedLength |
  IndeterminateLength
  deriving (Show, Read, Eq, Enum)
           
packetLengthCoding = [
  (OneOctedLength, 0),
  (TwoOctedLength, 1),
  (FourOctedLength, 2),
  (IndeterminateLength, 3)
  ]

lookupPacketLength :: Word8 -> Maybe PacketLength
lookupPacketLength i = lookup i $ map swap packetLengthCoding
packetLengthToNum :: PacketLength -> Maybe Word8
packetLengthToNum p = lookup p packetLengthCoding

                      
\end{code}

   The packet tag denotes what type of packet the body holds.  Note that
   old format headers can only have tags less than 16, whereas new
   format headers can have tags as great as 63.  The defined tags (in
   decimal) are as follows:

       0        -- Reserved - a packet tag MUST NOT have this value
       1        -- Public-Key Encrypted Session Key Packet
       2        -- Signature Packet
       3        -- Symmetric-Key Encrypted Session Key Packet
       4        -- One-Pass Signature Packet
       5        -- Secret-Key Packet
       6        -- Public-Key Packet
       7        -- Secret-Subkey Packet
       8        -- Compressed Data Packet
       9        -- Symmetrically Encrypted Data Packet
       10       -- Marker Packet
       11       -- Literal Data Packet
       12       -- Trust Packet
       13       -- User ID Packet
       14       -- Public-Subkey Packet
       17       -- User Attribute Packet
       18       -- Sym. Encrypted and Integrity Protected Data Packet
       19       -- Modification Detection Code Packet
       60 to 63 -- Private or Experimental Values

\begin{code}
data PacketTag = 
  PublicKeyEncryptedSessionKeyPacket |
  SignaturePacket |
  SymmetricKeyEncryptedSessionKeyPacket |
  OnePassSignaturePacket |
  SecretKeyPacket |
  PublicKeyPacket |
  SecretSubkeyPacket |
  CompressedDataPacket |
  SymmetricallyEncryptedPacket |
  MarkerPacket |
  LiteralDataPacket |
  TrustPacket |
  UserIDPacket |
  PublicSubkeyPacket |
  UserAttributePacket |
  SymEncryptedAndIntegrityProtectedDataPacket |
  ModificationDetectionCodePacket 
  deriving (Show, Read, Eq)  
           
packetTagCoding = [
  (PublicKeyEncryptedSessionKeyPacket, 1),
  (SignaturePacket, 2),
  (SymmetricKeyEncryptedSessionKeyPacket, 3),
  (OnePassSignaturePacket, 4),
  (SecretKeyPacket, 5),
  (PublicKeyPacket, 6),
  (SecretSubkeyPacket, 7),
  (CompressedDataPacket, 8),
  (SymmetricallyEncryptedPacket, 9),
  (MarkerPacket, 10),
  (LiteralDataPacket, 11),
  (TrustPacket, 12),
  (UserIDPacket, 13),
  (PublicSubkeyPacket, 14),
  (UserAttributePacket, 17),
  (SymEncryptedAndIntegrityProtectedDataPacket, 18),
  (ModificationDetectionCodePacket, 19)
  ]

isValidOldTag :: PacketTag -> Bool
isValidOldTag t = case (packetTagToNum t) of
  Nothing -> False
  Just t' -> t' < 16
  
isValidOldTagNum :: Word8 -> Bool
isValidOldTagNum i = case (lookupPacketTag i) of
  Nothing -> False
  Just _ -> (i > 0) && (i < 16)
  
packetTagToNum :: PacketTag -> Maybe Word8
packetTagToNum t = lookup t packetTagCoding
lookupPacketTag :: Word8 -> Maybe PacketTag
lookupPacketTag i = lookup i $ map swap packetTagCoding

                  




\end{code}
