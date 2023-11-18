import Data.List (sort)

calcChange owed given =
  if change > 0
    then change
    else 0
  where
    change = given - owed

doublePlusTwo x = doubleX + 2
  where
    doubleX = x * 2

inc n = n + 1

double n = n * 2

square n = n ^ 2

f n = if n `mod` 2 == 0 then n - 2 else 3 * n + 1

sumSquareOrSquareSum x y =
  if sumSquare > squareSum
    then sumSquare
    else squareSum
  where
    sumSquare = x ^ 2 + y ^ 2
    squareSum = (x + y) ^ 2

sumSquareOrSquareSum' x y =
  let sumSquare = x ^ 2 + y ^ 2
      squareSum = (x + y) ^ 2
   in if sumSquare > squareSum
        then sumSquare
        else squareSum

overwrite x = (\x -> (\x -> (\x -> x) 4) 3) 2

counter x =
  let x = x + 1
   in let x = x + 1
       in x

counter' x =
  (\x -> x + 1) ((\x -> x + 1) ((\x -> x) x))

ifEvenInc n = if even n then n + 1 else n

ifEven myFunction x =
  if even x
    then myFunction x
    else x

ifEvenInc' n = ifEven (\x -> x + 1) n

author = ("Will", "Kurt")

names =
  [ ("Ian", "Curtis"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook"),
    ("Stephen", "Morris")
  ]
