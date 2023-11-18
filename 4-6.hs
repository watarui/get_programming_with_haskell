addressLetter name location =
  let locationFunction = getLocationFunction location
   in locationFunction name

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

compareNames name1 name2 =
  let lastName1 = snd name1
      lastName2 = snd name2
      firstName1 = fst name1
      firstName2 = fst name2
      resultLastName = compare lastName1 lastName2
      resultFirstName = compare firstName1 firstName2
   in if resultLastName == EQ
        then resultFirstName
        else resultLastName
