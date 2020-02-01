onlyEven [] = []
onlyEven (x:xs)
  | x `mod` 2 == 0  = x : onlyEven xs
  | otherwise   = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
  | x `mod` 2 /= 0 = x : onlyOdd xs
  | otherwise = onlyOdd xs


filter' :: (a->Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
  |p x = x : filter' p xs
  | otherwise = filter' p xs

onlyEven' = filter (\x -> x `mod` 2 ==0)
onlyOdd' = filter(\x -> x `mod` 2 /=0)

foldr :: (a->b->b) -> b -> [a] ->b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
