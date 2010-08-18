{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances, 
             UndecidableInstances, 
             FunctionalDependencies, 
             TypeSynonymInstances #-}

module Turing where

data Direction = L | R               
               deriving (Show, Read, Eq, Enum)
                        

class (Eq γ, Ord q) => Turing t q γ | t -> q, t -> γ where
    initState :: t -> q
    finalStates :: t -> [q]
    transitionF :: t -> Transition q γ 
    isFinalState :: t -> q -> Bool
    isFinalState t s = elem s $ finalStates t
    eval :: t -> [γ] -> [(q, TapeT γ)]
    eval t i = 
      let tape0 = TapeT 0 (map Just i)
          state0 = initState t
          eval' tape state =
            let r = tapeRead tape
                (state', γ', d) = transitionF t state r 
                tape' = case d of 
                  L -> tapeLeft  $ tapeWrite tape γ' 
                  R -> tapeRight $ tapeWrite tape γ'
                step = (state, tape)
            in if (isFinalState t state) then [] else
                  step : (eval' tape' state')
      in eval' tape0 state0
           
type TuringT q γ = (q, [q], Transition q γ)

instance (Ord q, Eq γ) => Turing (TuringT q γ) q γ where
  initState (q, _, _) = q
  finalStates (_, fs, _) = fs
  transitionF (_, _, f) = f
                                      
type Transition q γ = q -> Maybe γ -> (q, Maybe γ, Direction)


data TapeT γ =
  TapeT { 
    tapePos :: Int,
    tape :: [Maybe γ]
    }
  
class Tape t γ | t -> γ where
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



