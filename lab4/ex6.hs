data BinTree a = EmptyBT | Node a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (Node a lt rt) = a + sumBinTree lt + sumBinTree rt

data Expr a = Lit a | Add (Expr a) (Expr a)
                    | Subtract (Expr a) (Expr a)
                    | Product (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n ) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Subtract e1 e2) = eval e1- eval e2
eval (Product e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a-> String
show' (Lit a) = show a
show' (Add e1 e2) = "halo"
show' (Subtract e1 e2) = "cos tam"
show' (Product e1 e2) = "lalala"

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (Node r lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

preOrder :: BinTree a -> [a]
preOrder EmptyBT = []
preOrder (Node r lt rt) = r : preOrder lt ++ preOrder rt

inOrder :: BinTree a -> [a]
inOrder EmptyBT = []
inOrder (Node r lt rt) = inOrder lt ++ [r] ++ inOrder rt

postOrder :: BinTree a -> [a]
postOrder EmptyBT = []
postOrder (Node a lt rt) = postOrder lt ++ postOrder rt ++ [r]

mapBT :: (a->b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (Node r lt rt) = Node (f r) (mapBT f lt) (mapBt f rt)

insert :: (Ord a ) => a -> BinTree a -> BinTree a
insert x EmptyBT = Node x EmptyBT EmptyBt
insert x (Node r lt rt)
            | x == r = Node r lt rt
            | x < r = Node r (insert x lt) rt
            | otherwise = Node r lt (insert x rt)

occurs :: Eq a => a -> BinTree a -> Bool
occurs x EmptyBT = False
occurs x (Node r lt rt)
            | x == r = True
            | x < r = occurs x lt
            | otherwise = occurs x rt

count :: Ord a => a -> BinTree a -> Int
count x EmptyBT = 0
count x (Node r lt rt)
          if x==r then 1 else 0 + ( count x lt ) + ( count x rt )

minElemOf :: (Ord a, Fractional a) => BinTree a -> a
minElemOf EmptyBT = 1/0
minElemOf (Node r lt rt) =  min r (min (minElemOf lt) (minElemOf rt))

maxElemOf :: Ord a => a -> BinTree -> a
maxElemOf EmptyBT = -1/0
maxElemOf (Node r lt rt) = max r (max (maxElemOf lt) (maxElemOf rt))

    
