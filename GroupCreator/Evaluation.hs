module GroupCreator.Evaluation
( Condition(..)
, fitness
) where

import GroupCreator.Groupings
import GroupCreator.People
import Data.List
import Data.Ord
import Data.Map
--import Math.
 --(Map)
--import qualified Data.Map as Map

data Condition = SizeRestriction { groupSize :: Int } --make groups of this many people
               | Friends { person :: Int, friend :: Int } --these two want to be in the same group
               | Enemies { person :: Int, enemy :: Int } --these two don't want to be in the same group
               | Peers { attributeIndex :: Int } --if attribute is "sex", groups should try to be either all-males or all-females (only for ENUM attributes)
               | Mixed { attributeIndex :: Int } --if attribute is "sex", groups should try to be mixed (3 M 2 F is better than 4 M 1 F)
               deriving (Show)

fitness :: Double -> [Condition] -> People -> Grouping -> Double
fitness conditionIndex [] people (Grouping grouping) = 0
fitness conditionIndex (cond:conds) people (Grouping grouping) = (1 + conditionIndex) * (eval cond people grouping) + (fitness (conditionIndex + 1) conds people (Grouping grouping))

--the higher the number, the worse the result of this evaluation
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


-- FROM HERE, test stuff

conditions =
  [ SizeRestriction 3
  , Enemies 5 10
  , Peers 0
  ]

groupingA = Grouping [[8,10,0],[3,16,4,13],[7,20],[17,15,2],[12],[1,6,19],[9,14,18],[5,11]]
groupingB = Grouping [[11,8],[14,18,0,4],[2,15,1],[5,20,10],[12,3],[6,19,17],[9,13,16,7]]

{-
  21 people, keys 0 to 20
  attribute 0 is "sex": 0=male, 1=female
  attribute 1 is "age": e.g. 31
  attribute 2 is "height" in meters: e.g. 1.86
-}
people :: People
people = fromList
  [ ( 0, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 30), (2, ContinuousAttribute 1.86)])
  , ( 1, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 28), (2, ContinuousAttribute 1.69)])
  , ( 2, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 33), (2, ContinuousAttribute 1.63)])
  , ( 3, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 36), (2, ContinuousAttribute 1.77)])
  , ( 4, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 40), (2, ContinuousAttribute 1.63)])
  , ( 5, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 19), (2, ContinuousAttribute 1.84)])
  , ( 6, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 22), (2, ContinuousAttribute 1.77)])
  , ( 7, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 23), (2, ContinuousAttribute 1.65)])
  , ( 8, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 29), (2, ContinuousAttribute 1.53)])
  , ( 9, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 27), (2, ContinuousAttribute 1.71)])
  , (10, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 30), (2, ContinuousAttribute 1.86)])
  , (11, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 28), (2, ContinuousAttribute 1.69)])
  , (12, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 33), (2, ContinuousAttribute 1.63)])
  , (13, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 36), (2, ContinuousAttribute 1.77)])
  , (14, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 40), (2, ContinuousAttribute 1.63)])
  , (15, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 19), (2, ContinuousAttribute 1.84)])
  , (16, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 22), (2, ContinuousAttribute 1.77)])
  , (17, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 23), (2, ContinuousAttribute 1.65)])
  , (18, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 29), (2, ContinuousAttribute 1.53)])
  , (19, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 27), (2, ContinuousAttribute 1.71)])
  , (20, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 55), (2, ContinuousAttribute 1.98)])
  ]

main = do
  print $ "grouping A eval: " ++ show (fitness 0 conditions people groupingA)
  print $ "grouping B eval: " ++ show (fitness 0 conditions people groupingB)
