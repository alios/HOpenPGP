{-# LANGUAGE GADTs #-}

module NmeaParser
       ( ExternalState(..)
       , Result(..)
       , DiscardReason(..)
       ) where 

import Language.Atom
import Data.Word

data InternalState 
     = SIntReset
     | SIntProcessing
     | SIntChecksum
     | SIntChecksumC1
     | SIntChecksumC2
     | SIntCR
     | SIntError
     deriving (Enum, Show, Eq, Ord)

data Event where
     EChar :: Word8 -> Event
     Reset :: Event
     deriving (Show, Eq)

data ExternalState
     = SWaitingForData
     | SProcessingData
     | SDone
     deriving (Enum, Show, Eq)

data DiscardReason
     = WrongChecksum
     | SentenceTooLong
     | InvalidCharacter
     deriving (Enum, Show, Eq) 


data Result where
     RNA :: Result
     ROk :: Result
     RDiscarded :: DiscardReason -> Result


nmeaBufferSize = (82 :: Word8)
nmeaMaxArgs = (10 :: Word8)
nmeaEmptyBufferDesc = replicate (fromIntegral nmeaBufferSize) (0 :: Word8)
nmeaEmptyBuffer n = array n nmeaEmptyBufferDesc

nmeaBuffer name = atom name $ 
           do buffer <- nmeaEmptyBuffer $ name ++ ".buffer"
              offsets <- array "offsets" $ replicate (fromIntegral nmeaMaxArgs) (0 :: Word8) 
              lengths <- array "lengths" $ replicate (fromIntegral nmeaMaxArgs) (0 :: Word8)
              args <- word8 "args" 0
              let r = nmeaBufferReset (buffer, args, offsets, lengths) 
              return (buffer, args, offsets, lengths, r)

nmeaBufferReset (buffer, args, offsets, lengths) = atom "reset" $
           do resetArray buffer nmeaBufferSize
              resetArray offsets nmeaMaxArgs
              resetArray lengths nmeaMaxArgs
              args <== Const (0 :: Word8)
                     
resetArray a l =
           sequence [ (a ! (Const i)) <== Const (0 :: Word8)  | i <- [0  .. l] ]                            

e2w8 :: (Enum e) => e -> Word8
e2w8 = fromIntegral . fromEnum

parser :: Name -> E Bool -> E Word8 -> V Bool -> Atom (E Bool, E Word8, A Word8, A Word8, A Word8)
parser n reset i ia = atom n $ do
       b1@(buffer1, args1, offsets1, lengths1, reset1) <- nmeaBuffer "buffer1"
       b2@(buffer2, args2, offsets2, lengths2, reset2) <- nmeaBuffer "buffer2"      

       useBuffer1 <- bool "useBuffer1" useBuffer1Init
       state <- word8 "state" $ e2w8 stateInit
       n <- word8 "n" 0

       

       atom "reset" $ do
            cond reset
            useBuffer1 <== Const useBuffer1Init
            state <== Const (e2w8 stateInit)
            n <== Const 0
            reset1
            reset2

       return $ undefined
       where useBuffer1Init = True
             stateInit  = SIntReset

            

           
     
                   
