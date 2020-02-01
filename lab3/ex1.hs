sumWith :: Num a => (a -> a) ->[a] -> a
sumWith  f [] = 0
sumWith f (x:xs) = (f x) + sumWith f xs

sum'' = sumWith id
sumSqr'' = sumWith (^2)
sumCube'' = sumWith (^3)
sumAbs'' = sumWith abs


y = sumWith (^5) [1..15]

listLength :: Num a => [a] -> a
listLength list = (sumWith (\x -> 1) list)

prodWith' :: Num a => (a->a)->[a] -> a
prodWith' f [] = 1
prodWith' f (x:xs) = (f x) * prodWith' f xs

prodSqr'' = prodWith' (^2)
