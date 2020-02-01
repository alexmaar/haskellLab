
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

class Monad m where
  return :: a -> m a
  (>=>) :: (a-> mb) -> (b -> mc) -> (a -> mc)

instance Monad Maybe where
  (Just x) >>= k = k x
  Nothing >>= _ = Nothing
  (>>) = (*>)
  fail _ = Nothing
