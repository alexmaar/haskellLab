f x y = sqrt (x^2 + y^2)

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs


sumWith :: Num a => (a -> a) ->[a] -> a
sumWith  f [] = 0
sumWith f (x:xs) = (f x) + sumWith f xs

f1 = \x -> (x-2)
f2 = \ x y -> (sqrt(x^2 + y^2))

f3 = \x -> (2 *) x
f4 = \x -> x*2
f5 = \x -> 2^x
f6 = \x -> x ^2
f7 = \x -> (2/x)
f8 = \x -> x/3
g9 = \x -> 4-x

f10 = \x -> sqrt (x)
f11 = \x -> abs x
