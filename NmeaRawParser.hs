{-# LANGUAGE GADTs #-}

module NmeaParser (
       ) where 

import Language.Atom
import Data.Monoid (mempty)
import Data.Word
import Data.Char

import Nmea0183

data InternalState 
     = SIntReset
     | SIntProcessing
     | SIntChecksum
     | SIntChecksumC1
     | SIntChecksumC2
     | SIntCR
     | SIntError
     deriving (Enum, Show, Eq, Ord)

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
           sequence [(a ! (Const i)) <== Const (0 :: Word8) | i <- [0  .. l]]
           
e2w8 :: (Enum e) => e -> Word8
e2w8 = fromIntegral . fromEnum

c2w8 :: Char -> Word8
c2w8 = fromIntegral . ord 

nothing = atom "nothing" $ do return ()
                            
isIn :: (EqE t) => [t] -> E t -> E Bool
isIn es i =  and_ [ i ==. (Const e) | e <- es]

parser :: Name -> E Bool -> E Word8 -> V Bool -> 
          E Word8 -> A Word8 -> A Word8 -> 
          Atom ()
parser n reset i ia nf os ls = atom n $ do
  b1@(buffer1, args1, offsets1, lengths1, reset1) <- nmeaBuffer "buffer1"

  state <- word8 "state" $ e2w8 stateInit
  n <- word8 "n" 0
  nf <- word8 "n" 0
  fails <- word8 "fails" 0  
  resetint <- bool "resetint" True
  
  let fieldtrans = nothing
        atom "fieldtrans" $ do
        return undefined --(lengths ! (value args)) <== 
          
  
  {-

  let updateOffset = atom "updateOffset" $ do
        (offsets1 ! (value args1)) <== ((value n) + (Const 1)))
  -}
  
  let updateOffset = undefined
      
  let errorcond name = atom name $ do
        incr fails
        resetint <== true
                   
  let stateAtom n s a = atom n $ do 
        cond $ not_ $ value resetint
        cond $ (value state) ==. (Const $ e2w8 s)
        cond $ value ia
        a
        
  let readInput = atom "readInput" $ do
        cond $ value ia          
        ia <== false
        return $ i
  
  let toBuffer :: A Word8 -> E Word8 -> Atom ()
      toBuffer b i = do
        (b ! (value n)) <== i
        incr n
        
  let nextState :: (Show e, Enum e) => 
                   (E Word8 -> E Bool) -> 
                   (E Word8 -> Atom ()) -> e -> Atom ()
      nextState pred action e = atom (show e) $ do
        i' <- readInput
        cond $ pred i'
        toBuffer buffer1 i'
        action i

  atom "resetint" $ do
    cond $ reset
    resetint <== true
    
  atom "reset" $ do
    cond $ value resetint
    state <== Const (e2w8 stateInit)
    n <== Const 0    
    reset1
    resetint <== false
    
  atom "boverflow" $ do
    cond $ (value n) >=. (Const nmeaBufferSize)
    errorcond "bufferoverflow"
    
  atom "foverflow" $ do
    cond $ (value args1 >=. (Const nmeaMaxArgs))
  
  stateAtom "inReset" SIntReset $ do
    nextState 
      (isIn [startChar])
      updateOffset
      SIntProcessing 
    
  stateAtom "inProcessing" SIntProcessing $ do
    nextState (isIn validChars) (\_ -> nothing) SIntProcessing
    nextState (isIn [fieldSep]) (\_ -> fieldtrans) SIntProcessing
    nextState (isIn [checksumSep]) (\_ -> nothing) SIntChecksum
    nextState (isIn undefChars) (\_ -> errorcond "undefchar") SIntReset

    
    where stateInit = SIntReset
