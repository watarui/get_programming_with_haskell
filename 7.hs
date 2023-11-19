myGCD a b =
  if remainder == 0
    then b
    else myGCD b remainder
  where
    remainder = a `mod` b

myTail [] = []
myTail (_ : xs) = xs

myGCD' a 0 = a
myGCD' a b = myGCD' b (a `mod` b)
