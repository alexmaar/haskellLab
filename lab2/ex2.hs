isPalindrome :: [Char] -> Bool
isPalindrome s | reverse s == s = True
                | otherwise = False

getElementAtIdx ::[Char] -> Int -> Char
getElementAtIdx w i = head (drop i w )
