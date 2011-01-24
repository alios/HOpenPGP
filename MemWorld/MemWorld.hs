{-# LANGUAGE MultiParamTypeClasses, 
             FunctionalDependencies,
             TypeSynonymInstances, 
             Rank2Types
 #-}

--
-- requires the Graphalyze package. for installation execute:
-- cabal update; cabal install Graphalyze
--

-- | 'MemWorld' is a visualization tool for mems in sozial network 'Streams'
module MemWorld where

import Data.Graph.Analysis.Types
--data AGr e w = AGr e w

-- | 'SocialStreams' represents a source of 'Mems' of type 'm' 
class (Streams s, Mems m) => SocialStreams s m | s -> m where
  -- | 'stream' evaluates to a time ordered list of 'Mems' of type 't' where
  -- the first element of the list represents the 
  stream :: s -> [(m, TimeStamp)]
            

type TimeStamp = Integer


-- | 'Streams' is an abstraction for a source of 'Mems' like users 
-- twitter timeline (or the global one), facebook or soup.io.  
class Streams s

-- | 'Mems' represent mindunits like hashtags (Strings), videos, pictures
class Mems m
             
-- | a 'String' can be a mem f.e. 
instance Mems String

-- | A mem also represents a node in a 'MemGraph' with weighted
-- edges to all other 'Mems'. The weights minimum represents
-- no "memetic" connection between two 'Mems' whereas the weights
-- maximum represents that two 'Mems' are the same - f.e. a image which
-- also has a representation as a hash tag.
type MemGraph mem w = (Mems mem, Bounded w, Ord w) => AGr m w

-- | 'Analyzers' map 'MemGraphs' on a ordered Lists of 'Mems' - Weight   
-- - tupel, which can be used as starting point for visualization 
-- together with the weighted graph structure itself 
class (Ord wmem) => Analyzers wedge wmem where
  -- | do the anlysis.
  analyze :: MemGraph mem wedge -> [(mem, wmem)]
  

-- with a lambda kiss... whatever it is 




-- -*- indent-tabs-mode: nil -*-
