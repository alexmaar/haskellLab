sgn :: Int -> Int
sgn n = if n < 0
	then -1
	else if n == 0
		then 0
		else 1

absInt :: Int -> Int
absInt n = if n < 0
	   then (-n)
	   else n

min2Int :: (Int, Int) -> Int
min2Int (x, y) = if x<=y
		then x
		else y

min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) = if min2Int(x, y) < z
		  then min2Int(x, y)
		  else z

toUpper :: Char -> Char
toUpper x = if x<= 'z' && x>= 'a'
	then toEnum(fromEnum(x) - 32)::Char
	else x 

toLower :: Char -> Char
toLower x = if x<='Z' && x>= 'A'
	then toEnum (fromEnum(x) + 32)::Char
	else x


isDigit :: Char -> Bool
isDigit x = x>= '0' && x<= '9'

charToNum :: Char -> Int
charToNum x = if x >= '0' && x<= '9'
		then fromEnum(x) - fromEnum('0')
		else -1

romanDigit :: Char -> String
romanDigit x = 
	if x=='1'
		then "I"
	else if x=='2'
	then "II"
	else if x == '3'
		then "III"
	else if x == '4'
		then "IV"
	else if x == '5'
		then "V"
	else if x == '6'
		then "VI"
	else if x == '7'
		then "VII"
	else if x == '8'
		then "VIII"
	else if x =='9'
		then "IX"
	else " "