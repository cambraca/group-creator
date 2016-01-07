module GroupCreator.Evaluation
( Condition(..)
, fitness
) where

import GroupCreator.Groupings
import GroupCreator.People
import Data.List
import Data.Ord
import Data.Map
import Data.Hash (hash, asWord64)

data Condition = SizeRestriction { groupSize :: Int }
                 --make groups of this many people
               | Friends { person :: Int, friend :: Int }
                 --these two want to be in the same group
               | Enemies { person :: Int, enemy :: Int }
                 --these two don't want to be in the same group
               | Peers { attributeIndex :: Int }
                 --if attribute is "sex", groups should try to be either all-males or all-females (only for ENUM attributes)
               | Mixed { attributeIndex :: Int }
                 --if attribute is "sex", groups should try to be mixed (3 M 2 F is better than 4 M 1 F)
               deriving (Show)

fitness :: [Condition] -> People -> Grouping -> Double
fitness conditions people grouping = run 0 conditions people grouping
  where
    run conditionIndex [] people (Grouping grouping) = fromIntegral (asWord64 $ hash grouping) / 1e25
    run conditionIndex (cond:conds) people (Grouping grouping) = (1 + conditionIndex) * (eval cond people grouping) + (run (conditionIndex + 1) conds people (Grouping grouping))

-- The higher the number, the worse the result of this evaluation
eval :: Condition -> People -> [[Int]] -> Double
eval (SizeRestriction groupSize) people [] = 0
eval (SizeRestriction groupSize) people (group:groups) = 0.5 * fromIntegral (abs (length group - groupSize)) + (eval (SizeRestriction groupSize) people groups)
eval (Friends person friend) people [] = 0
eval (Friends person friend) people (group:groups)
  | intersection == 2 = 0 --person and friend are in the same group
  | intersection == 1 = 5 --either person or friend are alone in the group
  | otherwise = eval (Friends person friend) people groups
  where
    intersection = length $ intersect group [person, friend]
eval (Enemies person enemy) people grouping = 5 - eval (Friends person enemy) people grouping
eval (Peers attributeIndex) people [] = 0
eval (Peers attributeIndex) people (firstGroup:groups) = (theRest / majorityCount) + (eval (Peers attributeIndex) people groups)
  where
    groupAttributeValues = Data.List.map (enumValue . (! attributeIndex)) $ Data.List.map (people !) firstGroup
    majorityCount = fromIntegral $ length $ head . sortBy (flip $ comparing length) . group . sort $ groupAttributeValues
    theRest = fromIntegral (length firstGroup) - majorityCount
