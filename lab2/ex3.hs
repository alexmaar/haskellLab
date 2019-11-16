--length[ (a,b,c) | a<-[1..100], b<-[1..100], c<-[1..100], a^2 +b^2==c^2]

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

--length [a | a<-[1..1000] ,[ a | i<-[2..a-1] , a `mod` i /=0] /= []]

primes :: [Integer]
primes = eratoSieve [2..]
  where
    eratoSieve :: [Integer] ->[Integer]
    eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]
