primes :: [Int]
primes = eratosieve [2..]
  where
    eratosieve :: [Int]->[Int]
    eratosieve (p : xs) = p : eratosieve [x | x<-xs,x `mod` p/= 0]

fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)
