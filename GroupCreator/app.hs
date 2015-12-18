import GroupCreator.Evaluation
import GroupCreator.Populations
import GroupCreator.People
import Data.Map

main = do
  let p = randomPopulation conditions people
  print $ bestIn p
  let p' = newPopulation p
  print $ bestIn p'

conditions =
  [ {-SizeRestriction 4
  , Friends 5 10
  , -}Peers 0
  ]

{-
  30 people, keys 0 to 29
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
  , (21, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 28), (2, ContinuousAttribute 1.69)])
  , (22, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 33), (2, ContinuousAttribute 1.63)])
  , (23, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 36), (2, ContinuousAttribute 1.77)])
  , (24, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 40), (2, ContinuousAttribute 1.63)])
  , (25, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 19), (2, ContinuousAttribute 1.84)])
  , (26, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 22), (2, ContinuousAttribute 1.77)])
  , (27, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 23), (2, ContinuousAttribute 1.65)])
  , (28, fromList [(0, EnumAttribute 0), (1, DiscreteAttribute 29), (2, ContinuousAttribute 1.53)])
  , (29, fromList [(0, EnumAttribute 1), (1, DiscreteAttribute 27), (2, ContinuousAttribute 1.71)])
  ]
