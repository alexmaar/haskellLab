data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show,Read,Ord,Eq)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node r lt rt)
  | x == r = Node r lt rt
  | x < r = Node r (treeInsert x lt) rt
  | x > r = Node r lt (treeInsert x rt)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a lt rt)
    | x == a = True
    | x < a = treeElem x lt
    | x > a = treeElem x rt

let nums = [8,6,4,1,7,3,5]
let numsTree = foldr treeInsert EmptyTree nums
