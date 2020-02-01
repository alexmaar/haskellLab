echo1 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho1 = do
  line <- getLine
  putStrLn $ line ++ "!"

echo3 :: IO ()
echo3 =  getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO ()
dialog = putStr "What is your happy number? "
           >> getLine
           >>= \n -> let num = read n :: Int in
                     if num == 7
                     then putStrLn "Ah, lucky 7!"
                     else if odd num
                          then putStrLn "Odd number! That's most people's choice..."
                          else putStrLn "Hm, even number? Unusual!"
doDialog :: IO ()
doDialog = do
  putStr "Whats is your happy number ?"
  num <-getLine
  let n = read num :: Int
  if n ==7
          then putStrLn "ah Lucky 7"
          else if odd n
              then putStrLn "Odd"
              else putStrLn "even"

twoQuestions' :: IO ()
twoQuestions' = putStr "What's your name ?"
                >> getLine >>= \name -> putStr "How old are you" >> getLine >>= \age -> print(name,age)



twoQuestions :: IO ()
twoQuestions = do
  putStr "What's your name ? "
  name <- getLine
  putStr "How old are you ? "
  age <- getLine
  print (name,age)
