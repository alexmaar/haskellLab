sqr:: Double -> Double
sqr x = x ^ 2

lengthIn2D:: (Double, Double) -> Double
lengthIn2D (x,y) = sqrt( x^2 + y^2 ) 

lengthIn3D:: (Double, Double, Double) -> Double
lengthIn3D (x,y,z) = sqrt(x^2 + y^2 + z^2)

swap:: (Int, Char) -> (Char, Int)
swap (int, char) = (char, int)

threeEqual::(Int, Int, Int) -> Bool
threeEqual (x,y,z) = x == y && y == z