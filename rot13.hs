module Main (main, rot13) where

import System.IO

chars = rot ['A'..'Z'] ++ rot ['a'..'z']

rot13 :: Char -> Char
rot13 c =
  let r = lookup c chars
  in case r of
    Nothing -> c
    Just c' -> c'
    
main :: IO ()
main = do
  eof <- hIsEOF stdin
  if (eof) then return ()
    else do c <- getChar
            putChar $ rot13 c
            main


rot l =
  let a1 = cycle l
      a2 = drop 13 a1
  in take 26 $ zip a1 a2
