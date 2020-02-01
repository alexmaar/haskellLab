{-# LANGUAGE DeriveFunctor #-}
doActSeq = do
  putChar 'A'
  putChar 'G'
  putChar 'H'
  putChar '\n'

newtype Box a = MkBox a deriving (Show,Functor)

data MyList a = EmptyList
              | Cons a (MyList a) deriving Show

instance Functor MyList where
  fmap _ EmptyList = EmptyList
  fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

instance Functor BinTree where
    fmap _ EmptyBT = EmptyBT
    fmap f (NodeBT a lt rt) = NodeBT (f a) (fmap f lt) (fmap f rt)

newtype Pair b a = Pair {getPair :: (a,b) }

instance Functor (Pair b) where
  fmap f (Pair (a,b)) = Pair(f a , b)

data Tree2 a = EmptyT2 | Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree2 where
  fmap _ EmptyT2 = EmptyT2
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node lt a rt) = Node (fmap f lt) (f a) (fmap f rt)

data GTree a = GLeaf a | GNode [Gtree a] deriving Show

instance Functor GTree where
    fmap f (GLeaf a) = GLeaf (f a)
    fmap f (GNode xs) = GNode (fmap (fmap f) xs)
