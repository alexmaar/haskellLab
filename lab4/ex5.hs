data MyInt = MkMyInt Int

instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1==i2

instance Ord MyInt where
    (<=) (MkMyInt i1) (MkMyInt i2) = i1<= i2

instance Num MyInt where
    (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
    (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
    (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
    negate (MkMyInt i1)  = MkMyInt (negate i1)
    abs (MkMyInt i1) = MkMyInt (abs i1)
    signum (MkMyInt i1) = MkMyInt (signum i1)

instance Show MyInt where
      show (MkMyInt i) = "MkMyInt " ++ show i

data BinTree a = NodeBT BinTree a BinTree a | Leaf  a

mapBT :: (a->b) -> BinTree a -> BinTree b
mapBT f (Leaf a) =  Leaf (f a)
mapVT f (NodeBT lt rt r) = NodeBT (map f lt) (mapBt f rt) (f r)

depthOfBT :: BinTree a -> Int
depthOfBT  (Leaf _) = 1
depthOfBT  NodeBT r lt rt = 1+ max (depthOfBT lt)  (depthofBT rt)
