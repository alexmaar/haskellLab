safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

extractMaybe :: Maybe a -> a
extractMaybe Nothing = error "nothing here "
extractMaybe (Just x) = x

insertMaybe :: a -> Maybe a
insertMaybe a = Just a

(>$>) :: a -> (a->b) -> b
x >$> f = f x
infixl 0 >$>

(>^$>) :: Maybe a -> (a->Maybe b) -> Maybe b
ma >^$> f = (extractMaybe ma) >$> f
infixl 1 >^$>

f1 :: (Ord a, Num a) => a -> Maybe a
f1 x = if x > 0 then Just (x +1) else Nothing

f2 :: (Eq a, Num a) => a -> Maybe a
f2 x = if x /= 0 then Just (10 * x) else Nothing

(>.>>) :: (a -> Maybe b ) -> (b -> Maybe c) -> (a -> Maybe c)
f >.>> g = \x -> g (extractMaybe (f x))

f >.>> g = \x -> (f x) >^$> g
