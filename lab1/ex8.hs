not' :: Bool -> Bool
not' b = case b of
	True -> False
	False -> True

absInt :: Int -> Int
absInt n = case (n>=0) of
	True -> n
	_    -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer a = case a of
	      "Love" -> True
	       _     -> False

or' :: (Bool, Bool) -> Bool
or' b = case b of 
	(True, True) -> True
	(False, True) -> True
	(True, False) -> True
	(False, False) -> False

xor' :: (Bool, Bool) -> Bool
xor' b = case b of 
	(True, True) -> False
	(True, False) -> True
	(False, True) -> True
	(False, False) -> False