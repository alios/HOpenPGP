> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

> import Data.Set (Set)
> import qualified Data.Set as S
> import Data.Maybe (fromJust)

A deterministic finite automaton

> type Transition q s = (q, s) -> q
> type TransitionTable q s = [((q, s), q)]

> class (Ord q, Ord s, Eq s) => DFA m q s | m -> q, m -> s where
>   states :: m -> Set q
>   inputsymbols :: m -> Set s
>   transition :: m -> Transition q s 
>   startState :: m -> q
>   acceptingStates :: m -> Set q
>   transitionTable :: m -> TransitionTable q s
>   run' :: m -> q -> [s] -> TransitionTable q s
>   run' _ _ [] = []
>   run' m s (i:is) = 
>     let nextstate = transition m (s, i)          
>     in ((s,i),nextstate) : run' m nextstate is
>   run :: m -> [s] -> TransitionTable q s
>   run m is = run' m (startState m) is
>   accepts :: m -> [s] -> Bool
>   accepts m is =
>     let (_, s) = last $ run m is 
>     in S.member s $ acceptingStates m
>   


> data (Ord q, Ord s, Eq s) => DFAT q s = DFA (TransitionTable q s) q (Set q)
>   deriving (Show)

> mkDFA :: (Ord q, Ord s, Eq s) => 
>            (TransitionTable q s) -> q -> Set q -> DFAT q s
> mkDFA tbl start accepting =
>   let dfa = DFA tbl start accepting  
>       is = inputsymbols dfa 
>       ss = states dfa
>       startIn = S.member start ss
>       acceptingIn = (acceptingStates dfa) `S.isSubsetOf` ss 
>       
>   in if (not startIn) 
>        then error "start State not in DFAs states"
>        else if (not acceptingIn) 
>         then error "accepting state is not a subset of the DFAs states"
>         else dfa
                                                                               
> instance (Ord q, Ord s, Eq s) => DFA (DFAT q s) q s where
>   startState (DFA _ q _) = q
>   acceptingStates (DFA _ _ fs) = fs
>   transitionTable (DFA ts _ _) = ts  
>   states (DFA ts _ _) = states' ts
>     where states' [] = S.empty
>           states' (((a, _), b):ts) =
>             (S.fromList [a,b]) `S.union` (states' ts) 
>   inputsymbols (DFA ts _ _) = inputsymbols' ts
>     where inputsymbols' [] = S.empty
>           inputsymbols' (((_, i), _):ts) = S.insert i (inputsymbols' ts) 
>   transition (DFA ts _ _) i = fromJust $ lookup i ts  



> tbl = [((0, '0'), 0)
>       ,((0, '1'), 1)
>       ,((1, '0'), 1)
>       ,((1, '1'), 2)
>       ,((2, '0'), 2)
>       ,((2, '1'), 1)
>       ]                  

> t1 = mkDFA tbl 0 (S.singleton 1)
> t2 = mkDFA tbl 3 (S.singleton 1)
> t3 = mkDFA tbl 0 (S.fromList [0,1])
> t4 = mkDFA tbl 0 (S.fromList [0,1,2,3,4])