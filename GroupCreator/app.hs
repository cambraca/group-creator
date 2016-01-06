import GroupCreator.Evaluation
import GroupCreator.Populations
import GroupCreator.People
import Data.Map
import Control.Monad.Random
import Control.Monad.Loops

-- These are percentages: 1 == 100%
elitismFactor = 0.05
crossoverFactor = 0.3
mutationFactor = 0.1
-- How big are populations?
populationSize = 100
maxPopulations = 500

selectTwo2 p = do
  ret <- getRandomR (0,size p)
  ret2 <- getRandomR (0,size p)
  return (ret, ret2)

main = do
--  let g = mkStdGen 1 --TODO seed value as parameter
  let elitism = ceiling $ fromIntegral populationSize * elitismFactor
  let p = randomPopulation conditions people populationSize :: Population

  let stopCondition (i, p) = i == maxPopulations || 0 == bestFitnessIn p
--  let evolve (i, p) = (newPopulation conditions people elitism crossoverFactor mutationFactor populationSize p) >>= (\p' -> (i + 1, p'))
  let evolve (i, p) = fmap (\p' -> (i + 1, p')) (newPopulation conditions people elitism crossoverFactor mutationFactor populationSize p)
  (number, new) <- iterateUntilM stopCondition evolve (0, p)

--  let new = newPopulation conditions people elitism crossoverFactor mutationFactor p g
--  new <- newPopulation conditions people elitism crossoverFactor mutationFactor populationSize p
--  new' <- newPopulation conditions people elitism crossoverFactor mutationFactor populationSize new
--  new'' <- newPopulation conditions people elitism crossoverFactor mutationFactor populationSize new'
--  new''' <- newPopulation conditions people elitism crossoverFactor mutationFactor populationSize new''

--  let (i, p') = until stopCondition iterate (0, p)
--  print $ new

--  putStrLn $ "size p is " ++ show (size p)
--  let (temp, g') = runRand (selectTwo2 p) g
--  let temp2 = evalRand (selectTwo2 p) g'
--  putStrLn $ "temp is " ++ show temp
--  putStrLn $ "temp is " ++ show temp2

---  print i
--  print p'
  print $ findMin p
  print $ findMin new
  print $ number
--  print $ findMin new'
--  print $ findMin new''
--  print $ findMin new'''

--  print elitism
--  let p' = newPopulation elitism crossoverFactor mutationFactor p
--  print $ bestIn p'

conditions =
  [ SizeRestriction 4
  , Friends 5 10
  , Peers 0
  ]

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
