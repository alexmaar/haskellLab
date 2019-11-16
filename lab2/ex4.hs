isPalindrome :: [Char] -> Bool
isPalindrome s  | s==reverse s = True
                | otherwise = False

getElemAtIdx :: [Int] -> Int ->Int
getElemAtIdx s w = head (drop w s)

let m = length [(a,b,c) | a<-[1..100], b<-[1..100], c<-[1..100], a^2 + b^2 == c^2]

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` 2 ==0]==[]

let n = length [n | n<-[1..100], [i | i<-[2..n-1], n `mod` i == 0 ] == []]
