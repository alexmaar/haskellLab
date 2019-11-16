--Zad 3 :
--x^2 | x<- [1..10], x `mod` 2 ==0 ]

--Zad 4 :

iloczyn :: [Int]-> Int
iloczyn x = loop 1 x
        where loop acc [] = acc
              loop acc (x:xs) = loop (acc*x) xs

-- kartkowka 2

plus3 :: Int -> Int
plus3  = (+) 3

-- uncurry :: (a->b->c) ->(a,b)->c

[x^3 | x<-[1..10] x `mod` 2 /=0 ]
