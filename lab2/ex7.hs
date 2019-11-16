qSort :: Ord a => [a]->[a]
qSort [] = []
qSort (x:xs) = qSort (leftPart xs )++ [x] ++ qSort (rightPart xs)
 where
  leftPart xs = [y | y<- xs , y<=x]
  rightPart xs = [y | y<- xs, y>x]



concat' :: [[a]] -> [a]
concat' c =  [ x  | y <-c, x<-y ]

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:xs) = x ++ concat'' xs

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (x : []) = True
isSorted (x:y:ys) | x<=y = True && isSorted (y:ys)
                  | otherwise = False

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] =[]
zip' x [] = []
zip' [] x = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

unzip' :: [(a,b)] -> ([a],[b])
unzip' z = ([x | (x,_) <- z], [y | (_,y) <-z])

subList :: Eq a => [a] -> [a] -> Bool
subList [] _ = True
subList _ [] = False
subList x (y:ys) | x == (take (length x) (y:ys)) = True
                 | otherwise = False || subList x ys

fst2Eq :: Eq a => [a] -> Bool
fst2Eq  (x : y : _) | x == y = True
fst2Eq _  = False

dzielnik :: (Num a, Eq a) => [a] -> Bool
dzielnik (x : y : _) | y `mod` x == 0 = True
dzielnik _ = False
