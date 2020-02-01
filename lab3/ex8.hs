m=map (\x -> 2*x)
n=map (\x -> sqrt x)
l=map(\x -> lowerCase x)

doubleElem = map (*2)
sqrtElem = map (^2)
lowerCase = map toLower

double xs = [2*x | x <- xs ]
sqrtelem xs = [x^2 | x <- xs ]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x = map ($ x)

z=map (x+2) xs [(x+2) x | x <- xs]
