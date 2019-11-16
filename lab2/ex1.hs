add2T :: Num a => (a,a) -> a
add2T (x,y) = x+y

add2C :: Num a => a -> (a -> a)
add2C x y = x+y

add3C :: Num a=> a -> (a ->( a -> a))
add3C x y z = x + y + z

fiveToPower :: Integer -> Integer
fiveToPower = (5^)

toPower5 :: Num a => a ->a
toPower5 = (^5)

substr4From :: Num a =>a -> a
substr4From = flip (-) 4

substrNfrom5 :: Num a => a -> a
substrNfrom5 x = 5 - x

substr5Fromn :: Num a => a -> a
substr5Fromn  = flip (-) 5


let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]

-- zipWith funkcja, argumenty (tablice)
