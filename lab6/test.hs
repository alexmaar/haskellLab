fun = do
  putStrLn "Podaj imie : "
  s<- getLine
  putStrLn $ "Witaj " ++ s

fun' = putStrLn "Podaj imie : " >> getLine >>= \s -> putStrLn $ "Witaj " ++ s

data Tree a = Node (Tree a) (Tree a) | Leaf a

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node lt rt) = Node (fmap f lt) (fmap f rt)

data Tree2 a = Node2 a (Tree2 a) (Tree2 a) | Leaf2

instance Functor Tree2 where
  fmap _ (Leaf2) = Leaf2
  fmap f (Node2 r lt rt ) = Node2 (f r) (fmap f lt) (fmap f rt)

main = do
    a<- return "a"
    b<- return "b"
    return ()
    return 1
    putStrLn $ a ++ " " ++ b

main' = do
    let a = "a"
        b = "b"
    return ()
    return 1
    putStrLn $ a ++ " " ++ b    
