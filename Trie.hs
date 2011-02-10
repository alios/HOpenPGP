{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

module Trie where

import qualified Data.Map as M


class Tries k v where
  data Trie k v
  tempty :: Trie k v
  tlookup :: Trie k v -> [k] -> Maybe v
  tbind :: [k] -> v -> Trie k v -> Trie k v
  tshow :: (Show k, Show v) => Trie k v -> String
  
instance (Ord k) => Tries k v where
  data Trie k v = Trie (M.Map k (Trie k v)) (Maybe v)
  
  tempty = Trie M.empty Nothing

  tlookup (Trie _ v) [] = v
  tlookup (Trie edges v) (k:ks) =
    case (M.lookup k edges) of
      Nothing -> Nothing
      Just t -> tlookup t ks
  
  tbind [] v (Trie edges _) = Trie edges $ Just v
  tbind (k:ks) nv (Trie edges v) =
    let t = case (M.lookup k edges) of
          Nothing -> tempty
          Just t' -> tbind ks nv t'
        newedges = M.insert k t edges
    in  (Trie newedges v) 

  tshow (Trie edges v) =
    let cs = M.toList edges
        a = concat [ "(" ++ show k ++ ", " ++ tshow ts ++ ")"  | (k, ts) <- cs] 
    in "(" ++ show v ++ ": [" ++ a ++ "])" 
                 
a = tbind "car" 30 $ tempty
x = tbind "cocktail" 10 $ tbind "cart" 20 $ a

y = tbind "" 23 $ tempty