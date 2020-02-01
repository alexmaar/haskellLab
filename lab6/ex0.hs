actSeq = putChar 'A' >> putChar 'G' >> putChar 'H'>> putChar '\n'

echo1 = do
  line <- getLine
  putStrLn line

echo2 = getLine >>= putStrLn

echo3 = getLine >>= \line -> putStrLn $ line ++ "!"

echo4 :: IO ()
echo4 = getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving Show

instance Functor BinTree where
  fmap _ EmptyBT = EmptyBT
  fmap f (NodeBT a lt rt) = NodeBT (f a) (fmap f lt) (fmap f rt)

newtype Box a = MkBox a deriving Show

instance Functor Box where
  fmap f (MkBox a) = MkBox (f a)

instance Applicative Box where
  pure a = MkBox a
  (MkBox f) <*> w = fmap f w
