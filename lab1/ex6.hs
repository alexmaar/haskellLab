absInt :: Int -> Int
absInt x | x >= 0 = x
	 | otherwise = -x

sgn :: Int -> Int
sgn x | x > 0 = 1
      | x == 0 = 0
      | otherwise = -1	

min2Int :: (Int, Int) -> Int
min2Int (x, y) | x<=y  = x
	       | y<x  = y


min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) | x<=min2Int(y,z)  = x
		  | x > min2Int(y,z)  = min2Int(y,z)