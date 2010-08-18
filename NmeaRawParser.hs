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
isIn es i =  any_ (==. i) $ map Const es

xor :: (IntegralE a) => E a -> E a -> E a
xor a b = (a `BWOr` b) `BWAnd` (BWNot (a `BWAnd` b))

hexChars :: [Word8]
hexChars = map c2w8 "0123456789ABCDEF"

cr :: Word8
cr = 0x23

lf :: Word8
lf = 0x24

copyArray :: (Assign t) => Word8 -> A t -> A t -> Atom ()
copyArray n a b = sequence [ ((b ! (Const i)) <== (a !. (Const i))) | i <- [0..n]] >> return ()
  
parser :: Name -> E Bool -> E Word8 -> V Bool -> 
          A Word8 -> V Word8 -> A Word8 ->  A Word8 ->
          Atom (V Bool)
parser name reset i ia buffer2 args2 offsets2 lengths2 = 
  let stateInit = SIntReset 
  in atom name $ do
  b1@(buffer1, args1, offsets1, lengths1, reset1) <- nmeaBuffer "buffer1"

  state <- word8 "state" $ e2w8 stateInit
  n <- word8 "n" 0
  fails <- word8 "fails" 0  
  resetint <- bool "resetint" True
  cs <- word8 "checksum" 0
  cs1 <- word8 "cs1" 0
  cs2 <- word8 "cs1" 0
  gotchecksum <- bool "gotchecksum" False
  done <- bool "done" False
  
  let updateChecksum i = atom "updateChecksum" $ do
        cs <== (value cs) `xor` i
      
  let errorcond name = atom name $ do
        incr fails
        resetint <== true
                       
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
                   (E Word8 -> Atom ()) -> e -> 
                   (E Word8) -> Atom ()
      nextState pred action e i = atom (show e) $ do
        cond $ pred i
        action i
        state <== (Const $ e2w8 e)

  let stateAtom name s action = atom name $ do 
        cond $ not_ $ value resetint
        cond $ (value state) ==. (Const $ e2w8 s)
        i <- readInput
        toBuffer buffer1 i
        sequence [a i | a <- (action ++ [invchr]) ]
        return ()
        where invchr = nextState 
                       (isIn undefChars) 
                       (\_ -> errorcond "undefchar") 
                       stateInit        
                       
  atom "resetint" $ do
    cond $ reset
    resetint <== true
    
  atom "reset" $ do
    cond $ value resetint
    state <== Const (e2w8 stateInit)
    n <== Const 0    
    cs <== Const 0
    cs1 <== Const 0
    cs2 <== Const 0
    gotchecksum <== false
    reset1
    resetint <== false
    
  atom "boverflow" $ do
    cond $ (value n) >=. (Const nmeaBufferSize)
    errorcond "bufferoverflow"
    
  atom "foverflow" $ do
    cond $ (value args1 >=. (Const nmeaMaxArgs))
  
      
  let normalchar i = updateChecksum i

  let fieldtrans i = 
        let l = (value n) - (offsets1 !. (value args1)) 
            os n = (offsets1 ! n)
            ls n = (lengths1 ! n)
        in atom "fieldtrans" $ do 
          updateChecksum i 
          (ls (value args1)) <== l
          (os $ (value args1) + (Const 1)) <== ((value n) + (Const 1))
          incr args1
              

  let finished = do 
        copyArray nmeaBufferSize buffer1 buffer2
        copyArray nmeaMaxArgs offsets1 offsets2
        copyArray nmeaMaxArgs lengths1 lengths2
        args2 <== value args1
        done <== true
  
  stateAtom "inReset" SIntReset
    [ nextState (isIn [startChar]) 
      (\_ -> (offsets1 ! (value args1) <== (Const 1)))
      SIntProcessing ]
  
  stateAtom "inProcessing" SIntProcessing 
    [ nextState (isIn [fieldSep]) fieldtrans SIntProcessing
    , nextState (isIn [checksumSep]) (\_ -> nothing) SIntChecksum
    , nextState (isIn validChars) normalchar SIntProcessing
    , nextState (isIn [cr]) (\_ -> nothing) SIntCR
    ]

  stateAtom "inChecksum" SIntChecksum
    [ nextState (isIn hexChars) (\i -> cs1 <== i) SIntChecksumC1
    ]

  stateAtom "inChecksumC1" SIntChecksumC1  
    [ nextState (isIn hexChars) (\i -> cs2 <== i) SIntChecksumC2
    ]
    
  stateAtom "inChecksumC2" SIntChecksumC2  
    [ nextState (isIn [cr]) (\_ -> gotchecksum <== true) SIntCR
    ]

  stateAtom "inCR" SIntCR
    [ nextState (isIn [lf]) (\_ -> finished) stateInit
    ]
    
  return done
