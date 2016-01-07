import GroupCreator.Evaluation
import GroupCreator.Populations
import GroupCreator.Groupings
import GroupCreator.People
import Data.Map
import Control.Monad.Random
import Control.Monad.Loops
import Control.Monad (replicateM)

-- These are percentages: 1 == 100%
elitismFactor = 0.05
crossoverFactor = 0.3
mutationFactor = 0.07
-- How big are populations?
populationSize = 100
maxPopulations = 300

selectTwo2 p = do
  ret <- getRandomR (0,size p)
  ret2 <- getRandomR (0,size p)
  return (ret, ret2)

tests count elitismFactor crossoverFactor mutationFactor populationSize maxPopulations = do
  let run = main2 elitismFactor crossoverFactor mutationFactor populationSize maxPopulations
  results <- replicateM count run
  print results
  let results2 = Prelude.map fst results
  print results2
  print $ sum results2 / (fromIntegral $ length results2)

main2 elitismFactor crossoverFactor mutationFactor populationSize maxPopulations = do
  let elitism = ceiling $ fromIntegral populationSize * elitismFactor
  p <- randomPopulation conditions people populationSize

  let stopCondition (i, p) = i == maxPopulations || 0 == bestFitnessIn p
  let evolve (i, p) = fmap (\p' -> (i + 1, p')) (newPopulation conditions people elitism crossoverFactor mutationFactor populationSize p)
  (generationsRan, new) <- iterateUntilM stopCondition evolve (0, p)
  return $ findMin new

main = do
--  let g = mkStdGen 1 --TODO seed value as parameter
  let elitism = ceiling $ fromIntegral populationSize * elitismFactor
  p <- randomPopulation conditions people populationSize

  let stopCondition (i, p) = i == maxPopulations || 0 == bestFitnessIn p
  let evolve (i, p) = fmap (\p' -> (i + 1, p')) (newPopulation conditions people elitism crossoverFactor mutationFactor populationSize p)
  (generationsRan, new) <- iterateUntilM stopCondition evolve (0, p)

  print $ findMin p
  print $ showAttr 0 (snd $ findMin p) people
  print $ findMin new
  print $ showAttr 0 (snd $ findMin new) people
  print $ generationsRan

conditions =
  [ SizeRestriction 3
  , Friends 5 10
  , Peers 0
  ]

showAttr which (Grouping grouping) people = grouping2
  where
    grouping2 = Prelude.map (\group -> Prelude.map (\person -> enumValue (people ! person ! which)) group) grouping

{-
  30 people, keys 0 to 29
  attribute 0 is "sex": 0=male, 1=female
  attribute 1 is "age": e.g. 31
  attribute 2 is "height" in meters: e.g. 1.86
-}
people :: People
people = Data.Map.fromList
  [ ( 0, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 30), (2, ContinuousAttribute 1.86)])
  , ( 1, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 28), (2, ContinuousAttribute 1.69)])
  , ( 2, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 33), (2, ContinuousAttribute 1.63)])
  , ( 3, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 36), (2, ContinuousAttribute 1.77)])
  , ( 4, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 40), (2, ContinuousAttribute 1.63)])
  , ( 5, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 19), (2, ContinuousAttribute 1.84)])
  , ( 6, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 22), (2, ContinuousAttribute 1.77)])
  , ( 7, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 23), (2, ContinuousAttribute 1.65)])
  , ( 8, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 29), (2, ContinuousAttribute 1.53)])
  , ( 9, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 27), (2, ContinuousAttribute 1.71)])
  , (10, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 30), (2, ContinuousAttribute 1.86)])
  , (11, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 28), (2, ContinuousAttribute 1.69)])
  , (12, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 33), (2, ContinuousAttribute 1.63)])
  , (13, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 36), (2, ContinuousAttribute 1.77)])
  , (14, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 40), (2, ContinuousAttribute 1.63)])
  , (15, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 19), (2, ContinuousAttribute 1.84)])
  , (16, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 22), (2, ContinuousAttribute 1.77)])
  , (17, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 23), (2, ContinuousAttribute 1.65)])
  , (18, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 29), (2, ContinuousAttribute 1.53)])
  , (19, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 27), (2, ContinuousAttribute 1.71)])
  , (20, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 55), (2, ContinuousAttribute 1.98)])
  , (21, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 28), (2, ContinuousAttribute 1.69)])
  , (22, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 33), (2, ContinuousAttribute 1.63)])
  , (23, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 36), (2, ContinuousAttribute 1.77)])
  , (24, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 40), (2, ContinuousAttribute 1.63)])
  , (25, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 19), (2, ContinuousAttribute 1.84)])
  , (26, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 22), (2, ContinuousAttribute 1.77)])
  , (27, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 23), (2, ContinuousAttribute 1.65)])
  , (28, Data.Map.fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 29), (2, ContinuousAttribute 1.53)])
  , (29, Data.Map.fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 27), (2, ContinuousAttribute 1.71)])
  ]
