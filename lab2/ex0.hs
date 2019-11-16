f :: Int -> Int
f =   ( 2 *)

-- zipWith :: (a-> b-> c) -> [a] -> [b] -> [c]

square = [x^2 | x<-[1..10], x `mod` 2 == 0]

g :: Int -> Int
g = (+) 3

--uncurry :: (a->b->c) -> (a,b) -> c

szescian = [x^3 | x<-[1..10], x `mod` 2 /= 0 ]


q=[ (3,j) | i<-[2,1], j<-[i..2]]
p=[[i^2, j^2] | i<-[1..2], j<-[1..2], j>1]



selectEven :: Integral a => [a] -> [a]
selectEven [] = []
selectEven (x:xs) | x `mod` 2 ==0 = x : selectEven xs
                  | otherwise = selectEven xs

product' :: Num a => [a] -> a
product'  = loop 1
    where loop acc []   = acc
          loop acc (x:xs)   = loop (x * acc) xs

eq :: Eq a => [a] -> Bool
eq (x : y : _) | x==y = True
eq _  =False

dzielnik (x : y : _) | y `mod` x ==0 = True
dzielnik _ = False

dzielnik3 (x : _ : y : _) | x `mod` y == 0 = True
dzielnik3 _ = False

sort' :: Ord a => [a] -> [a] -> [a]
sort' [][] = []
sort' [] ys = ys
sort' xs [] = xs
sort' (x:xs)(y:ys) | x<y = x: sort' xs (y:ys)
                   | otherwise = y : sort' (x:xs) ys

length'2 :: Num a => [a] -> a
length'2 = loop 0
      where loop acc [] = acc
            loop acc (x:xs) = loop (1+acc) xs
