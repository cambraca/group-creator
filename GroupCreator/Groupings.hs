module GroupCreator.Groupings
( Grouping(..)
, crossover
, mutate
) where

import Data.List

data Grouping = Grouping [[Int]]
  deriving (Eq, Show)

--a = Grouping [[8,10,0],[3,16,4,13],[7,20],[17,15,2],[12],[1,6,19],[9,14,18],[5,11]]
--b = Grouping [[11,8],[14,18,0,4],[2,15,1],[5,20,10],[12,3],[6,19,17],[9,13,16,7]]
crossover :: Grouping -> Grouping -> Grouping
crossover (Grouping a) (Grouping b) = Grouping $ firstPart `Data.List.union` secondPartWithoutDupes --[[8,10,0],[9,13,16,7],[17,15,2],[12,3],[1,6,19],[5,11],[4,20],[14,18]]
  where
    secondPartWithoutDupes = cleanUp $ removeDupes [] secondPart --[[4,20],[14,18]]
    secondPart = joinByPairs theRestWithoutSeen --[[4,20],[14,18,14,18,4],[20]]
    theRestWithoutSeen = cleanUp $ Data.List.map (\\ seenInFirst) theRest --[[4],[20],[14,18],[14,18,4],[20]]
    theRest = (a `Data.List.union` b) \\ firstPart --[[3,16,4,13],[7,20],[12],[9,14,18],[11,8],[14,18,0,4],[2,15,1],[5,20,10],[6,19,17]]
    seenInFirst = concat firstPart --[8,10,0,9,13,16,7,17,15,2,12,3,1,6,19,5,11]
    firstPart = interleaved [] a (reverse b) --[[8,10,0],[9,13,16,7],[17,15,2],[12,3],[1,6,19],[5,11]]

cleanUp :: Eq a => [[a]] -> [[a]]
cleanUp a = Data.List.filter (/=[]) a

removeDupes :: Eq a => [a] -> [[a]] -> [[a]]
removeDupes _ [] = []
removeDupes collector (x:xs) = (nub x \\ collector) : removeDupes (collector ++ nub x) xs

joinByPairs :: [[a]] -> [[a]]
joinByPairs [] = []
joinByPairs (x:[]) = (x:[])
joinByPairs (x:y:xs) = (x ++ y) : joinByPairs xs

interleaved :: Eq a => [a] -> [[a]] -> [[a]] -> [[a]]
interleaved acc [] [] = []
interleaved acc [] x = interleaved acc x []
interleaved acc (x:xs) y = if (x `intersect` acc) == []
                           then (x:interleaved (acc ++ x) y xs) --swap xs and y
                           else interleaved acc xs y

-- The three given Ints can be random integers
mutate :: Int -> Int -> Int -> Grouping -> Grouping
mutate sourceI destinationI personI (Grouping grouping)
  | source == destination = Grouping grouping
  | otherwise = Grouping $ replaceOnce destination destination' . replaceOnce source source' $ grouping
    where
      groupCount = length grouping
      source = grouping !! (mod sourceI groupCount)
      destination = grouping !! (mod destinationI groupCount)
      personCountInSource = length source
      person = source !! (mod personI personCountInSource)
      source' = source \\ [person]
      destination' = destination ++ [person]
      replaceOnce :: Eq a => a -> a -> [a] -> [a]
      replaceOnce _ _ [] = []
      replaceOnce a b (x:xs) = if x == a
        then b : xs
        else x : replaceOnce a b xs
