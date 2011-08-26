{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances, TemplateHaskell #-}

module Trie where

import Data.Maybe (isJust, fromJust, fromMaybe)
import qualified Data.Map as M
import Test.QuickCheck
import Test.QuickCheck.All

import Data.Word


class (Ord k) => Tries k v where
  data Trie k v
  tempty :: Trie k v
  tlookup :: Trie k v -> [k] -> Maybe v
  tbind :: [k] -> v -> Trie k v -> Trie k v
  tfromList :: [([k], v)] -> Trie k v
  tfromList = foldl (\t (k, v) -> tbind k v t) tempty
  
instance (Show v, Show k) => Show (Trie k v) where
  show (Trie m v) = show v ++ ": " ++ show m

 
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
    let t = fromMaybe tempty $ M.lookup k edges
        newedges = M.insert k (tbind ks nv t) edges
    in  (Trie newedges v) 

prop_bind :: (Ord k, Eq v) => [k] -> v -> Bool
prop_bind k v =
  let t = tbind k v tempty
      v' = tlookup t k
  in isJust v' && (fromJust v') == v
     
poly_prop_bind = $(polyQuickCheck 'prop_bind)

prop_bind_s :: String -> Int -> Bool
prop_bind_s = prop_bind

prop_fromList :: (Ord k, Eq v) => [([k], v)] -> Bool
prop_fromList m = 
  let map = M.fromList m
      lst = M.toList map
      trie = tfromList lst
  in and [ (tlookup trie k) == (Just v) | (k,v) <- lst ]
     
poly_prop_fromList = $(polyQuickCheck 'prop_fromList)

prop_fromList_s :: [(String, Int)] -> Bool
prop_fromList_s = prop_fromList

runTests = do
  poly_prop_bind
  poly_prop_fromList
  $quickCheckAll 
