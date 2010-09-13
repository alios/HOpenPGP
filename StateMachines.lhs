> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

> import Data.Set(Set)
> import qualified Data.Set as S

A deterministic finite automaton

> type Transition q s = (q, s) -> q
> type TransitionTable q s = [(q, s, q)]

> class (Ord q, Ord s, Eq s) => DFA m q s | m -> q, m -> s where
>   states :: m -> Set q
>   inputsymbols :: m -> Set s
>   transitionF :: m -> Transition q s 
>   startState :: m -> q
>   acceptingStates :: m -> Set q


> data (Ord q, Ord s, Eq s) => DFAT q s = DFA (TransitionTable q s) q (Set q)
>   deriving (Show)
                                                                                    
> instance (Ord q, Ord s, Eq s) => DFA (DFAT q s) q s where
>   startState (DFA _ q _) = q
>   acceptingStates (DFA _ _ fs) = fs
>   states (DFA ts _ _) = states' ts
>     where states' [] = S.empty
>           states' ((a, _, b):ts) = (S.fromList [a,b]) `S.union` (states' ts) 
>   inputsymbols (DFA ts _ _) = inputsymbols' ts
>     where inputsymbols' [] = S.empty
>           inputsymbols' ((_, i, _):ts) = S.insert i (inputsymbols' ts) 



> t = DFA tbl 0 (S.singleton 1)
>   where tbl = [(0, '0', 0)
>               ,(0, '1', 1)
>               ,(1, '0', 1)
>               ,(1, '1', 2)
>               ,(2, '0', 2)
>               ,(2, '1', 1)
>               ]                  