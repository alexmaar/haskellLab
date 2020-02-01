data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a ) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n+ sumBinTree lt + sumBinTree rt

data Expr a = Lit a |
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt ) =  1 + max (depthOfBT lt) (depthOfBT rt)

flattenBTInOrder :: BinTree a ->[a]
flattenBTInOrder EmptyBT = []
fattenBTInOrder (NodeBT n lt rt) = flattenBTInOrder lt ++ [n] ++ flattenBTInOrder rt

flattenBTPostOrder :: BinTree a -> [a]
flattenBTPostOrder EmptyBT = []
flattenBTPostOrder (NodeBT n lt rt ) =  flattenBTPostOrder lt ++ flattenBTPostOrder rt ++ [n]

flattenBTPreOrder :: BinTree a -> [a]
flattenBTPreOrder EmptyBT = []
flattenBTPreOrder (NodeBT n lt rt ) = [n] ++ flattenBTPreOrder lt ++ flattenBTPreOrder rt

insert :: Ord a => a -> BinTree a -> BinTree a
insert n EmptyBT = NodeBT n EmptyBT EmptyBT
insert n (NodeBT r lt rt) = if n > r
                              then NodeBT r lt (insert n rt)
                              else NodeBT r (insert n lt ) rt
