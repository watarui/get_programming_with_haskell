-- list
teams = ["red", "yellow", "orange", "blue", "purple"]

assignToGroups n aList =
  let groups = cycle [1 .. n]
   in zip groups aList

repeat i = cycle [i]

subseq begin end list =
  let diff = end - begin
   in take diff $ drop begin list

inFirstHalf e l =
  let mid = (length l) `div` 2
      firstHalf = take mid l
   in e `elem` firstHalf
