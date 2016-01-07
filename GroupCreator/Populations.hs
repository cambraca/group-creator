module GroupCreator.Populations
( Population(..)
, randomPopulation
, newPopulation
, selectTwo
, bestIn
, bestFitnessIn
, newP
, decide
) where

import Data.Map
import Data.List.Split (chunksOf)
import GroupCreator.Groupings as G
import GroupCreator.Evaluation as E
import GroupCreator.People
import Control.Monad
import Control.Monad.Random
import System.Random.Shuffle (shuffleM)

type Population = Map Double Grouping

populationFromList conditions people groupings = Data.Map.fromList $ Prelude.map convert groupings
  where convert a = (fitness conditions people a, a)

randomGrouping :: MonadRandom m => Int -> Int -> m Grouping
randomGrouping people groupSize = do
  shuffled <- shuffleM [0..people-1]
  let groups = chunksOf groupSize shuffled
  return $ Grouping groups

randomPopulation :: MonadRandom m => [Condition] -> People -> Int -> m Population
randomPopulation conditions people populationSize = do
  let groupSize = E.groupSize $ head [SizeRestriction a | SizeRestriction a <- conditions]
  groupings <- replicateM populationSize $ randomGrouping (size people) groupSize
  return $ populationFromList conditions people groupings

{-
  Params:
  * How many groupings to maintain (elitism)
  * Crossover probability, 0 means no crossover, 1 means always,
    a value of 0.5 means half the time there will be a crossover
    between selected pairs
  * Mutation probability, 0 means never, 1 means there will be X
    number of mutations on every child grouping, where X is the
    number of people. E.g. 0.01 means a grouping will have X*1%
    mutations, so for 100 people, there will probably always be 1
    mutation.
-}
newPopulation :: MonadRandom m => [Condition] -> People -> Int -> Double -> Double -> Int -> Population -> m Population
newPopulation conditions people elitism crossover mutation populationSize p = do
  (g1, g2) <- selectTwo p -- Selection

  let elitismGroupings = Prelude.map snd (take elitism $ toAscList p)
  groupings <- replicateM (populationSize - length elitismGroupings) (newGrouping crossover mutation p)

  let new = populationFromList conditions people (elitismGroupings ++ groupings)

  return new

newGrouping :: MonadRandom m => Double -> Double -> Population -> m Grouping
newGrouping crossover mutation p = do
  willDoCrossover <- decide crossover-- :: Bool
  selected <- selectTwo p-- :: (Grouping, Grouping)
  let result = G.crossover (fst selected) (snd selected) :: Grouping
  let new = if willDoCrossover then result else (fst selected) :: Grouping
  let peopleCount (Grouping grouping) = sum $ Prelude.map length grouping
  mutations <- replicateM (peopleCount new) (defineMutation mutation)
  let mutant = mutate mutations new
  return mutant

defineMutation probability = do
  willMutate <- decide probability
  a <- getRandomR (0, maxBound :: Int)
  b <- getRandomR (0, maxBound :: Int)
  c <- getRandomR (0, maxBound :: Int)
  let result = if willMutate then Just (a,b,c) else Nothing
  return result

main = do
  g <- getStdGen
  let r = evalRand (newP) g :: Double
  putStrLn $ "result " ++ show r

newP = do
  ret <- getRandomR (0.0,1.0)
  return ret

decide :: MonadRandom m => Double -> m Bool
decide probability = do
  value <- getRandomR (0.0, 1.0)
  return $ value < probability

selectTwo :: MonadRandom m => Population -> m (Grouping, Grouping)
selectTwo p = do
  let l' = size p
  let l = fromIntegral $ size p
  pos1 <- Control.Monad.Random.fromList $ zip [0..l'-1] [l,l-1..]
  pos2 <- Control.Monad.Random.fromList $ zip [0..l'-1] [l,l-1..]
  return $ (snd (elemAt pos1 p), snd (elemAt pos2 p))

bestIn :: Population -> Grouping
bestIn p = snd $ findMin p

bestFitnessIn :: Population -> Double
bestFitnessIn p = fst $ findMin p
