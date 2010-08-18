{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, RankNTypes #-}

module Turing where


data Direction = L | R
               deriving (Show, Read, Eq, Enum)
                        
class (Eq q, Eq γ, Ord q) => Turing t q γ where
    inputSyms :: t -> [γ]
    initState :: t -> q
    finalStates :: t -> [q]
    transitionF :: t -> Transition q γ 
    transit :: t -> q -> t
    isFinalState :: t -> q -> Bool
    isFinalState t s = elem s $ finalStates t
    eval :: t -> [γ] -> [(q, TapeT γ)]
    eval t i = 
      let tape = TapeT 0 (map Just i)
          q = initState t
          eval' t tape state =
            let r = tapeRead tape
                (q', γ', d) = transit t r 
                tape' = case d of 
                  L -> tapeLeft  $ tapeWrite tape γ' 
                  R -> tapeRight $ tapeWrite tape γ'
                step = (state, tape)
            in if (isFinalState state) then [] else
                 eval' t tape' q' 
      in eval' tape q
           
                                      
type Transition q γ = q -> γ -> (q, γ, Direction)


data TapeT γ =
  TapeT { 
    tapePos :: Int,
    tape :: [Maybe γ]
    }
  
class Tape t γ where
  tapeRead :: t -> Maybe γ 
  tapeWrite :: t -> Maybe γ -> t
  tapeLeft :: t -> t
  tapeRight :: t -> t
  
instance Tape (TapeT γ) γ where 
  tapeRead t = (tape t) !! (tapePos t)
  tapeWrite t v = 
    let pos = tapePos t 
        left  = take pos (tape t)
        (_:right) = drop pos (tape t)
    in TapeT pos (left ++ [v] ++ right) 
  tapeLeft t
   | (tapePos t == 0) = TapeT (tapePos t) (Nothing : (tape t))
   | otherwise = TapeT ((tapePos t) - 1) (tape t)
  tapeRight t
   | (((tapePos t) + 1) == (length $ tape t)) = 
       TapeT ((tapePos t) + 1) ((tape t) ++ [Nothing])
   | otherwise = TapeT ((tapePos t) + 1) (tape t)
