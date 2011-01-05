
module Main(main) where



rot l =
  let a1 = cycle l
      a2 = drop 13 a1
  in take 26 $ zip a1 a2

chars = rot ['A'..'Z'] ++ rot ['a'..'z']

rot13 c =
  let r = lookup c chars
  in case r of
    Nothing -> c
    Just c' -> c'
    


rot13str = map rot13

main = interact rot13str
