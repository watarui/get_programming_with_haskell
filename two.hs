import Data.List

author = ("Will", "Kurt")

names =
  [ ("Ian", "Curtis"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook"),
    ("Stephen", "Morris")
  ]

compareLastNames name1 name2 =
  let lastName1 = snd name1
      lastName2 = snd name2
   in if lastName1 > lastName2
        then GT
        else
          if lastName1 < lastName2
            then LT
            else EQ

compareNames name1 name2 =
  let lastName1 = snd name1
      lastName2 = snd name2
      firstName1 = fst name1
      firstName2 = fst name2
   in if lastName1 > lastName2
        then GT
        else
          if lastName1 < lastName2
            then LT
            else
              if firstName1 > firstName2
                then GT
                else
                  if firstName1 < firstName2
                    then LT
                    else EQ
