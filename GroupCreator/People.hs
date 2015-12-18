module GroupCreator.People
( Person(..)
, People(..)
, Attribute(..)
) where

import Data.Map (Map)
import qualified Data.Map as Map

type People = Map Int Person

data Attribute = EnumAttribute { enumValue :: Int } --e.g. sex (male, female, unknown)
               | DiscreteAttribute { discreteValue :: Int } --e.g. age (13, 16, 17, 22, ...)
               | ContinuousAttribute { continuousValue :: Double } --e.g. size (1.62, 1.85, ...)
               deriving (Show)

type Person = Map Int Attribute
--data Person = Person (Map Int Attribute) deriving (Show)

--data Person = Person { index :: Int
--                     , attributes :: [(Int, Attribute)]
--                     } deriving (Show)

--camilo = Person 0 [(0, (EnumAttribute 3)), (1, (DiscreteAttribute 30))]