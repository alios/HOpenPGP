
> import Data.Set(Set)

A deterministic finite automaton

> type Transition q z = (q, z) -> q
> type DFA q z = (Set q, Set z, Transition q z, q, Set z)

