ifEven f x =
  if even x
    then f x
    else x

ifEvenInc' = ifEven inc

genIfEven f = (\x -> ifEven f x)

inc n = n + 1

ifEvenInc = genIfEven inc

genIfXEven x = (\f -> ifEven f x)

getRequestUrl host apikey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apikey

genHostRequestBuilder host = (\apikey resource id -> getRequestUrl host apikey resource id)

exampleUrlBuilder = genHostRequestBuilder "http://example.com"

genApiRequestBuilder hostBuilder apiKey resource =
  (\id -> hostBuilder apiKey resource id)

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

add4 a b c d = a + b + c + d

addXto3 x = (\b c d -> add4 x b c d)

addXYto2 x y = (\c d -> add4 x y c d)

gen = myExampleUrlBuilder "book"

flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)

sfOffice name =
  let lastName = snd name
      nameText = (fst name) ++ " " ++ lastName
   in if lastName < "L"
        then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
        else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where
    nameText = snd name

getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "dc" ->
    ( \name ->
        let nameText = (fst name) ++ " " ++ (snd name) ++ ", Esq."
         in nameText ++ "PO Box 1337 - Washington DC, 20001"
    )
  _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location =
  let locationFunction = getLocationFunction location
   in locationFunction name

addressLetterV2 = flipBinaryArgs addressLetter

addressLetterNY = addressLetterV2 "ny"

subtract2 = flip (-) 2

binaryPartialApplication f x y = f x y
