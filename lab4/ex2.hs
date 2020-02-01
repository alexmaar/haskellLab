data Person = Person
              { name :: String
              , surname :: String
              } deriving (Show)

data Person' n s = Person' n s

data Either a b = Left a | Right b
