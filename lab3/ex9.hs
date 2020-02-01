sumWith' :: Num a => (a->a) -> [a] -> a
sumWith' = go 0
  where
    go acc g [] = acc
    go acc g (x : xs) = go (g x + acc ) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
  where
    go acc g [] = acc
    go acc g (x:xs) = go (g x * acc) g xs


  
