module GroupCreator.People
( Person(..)
, People(..)
, Attribute(..)
) where

import Data.Map (Map)
import qualified Data.Map as Map

type People = Map Int Person

type Person = Map Int Attribute

data Attribute = EnumAttribute { enumValue :: Int } --e.g. sex (male, female, unknown)
               | DiscreteAttribute { discreteValue :: Int } --e.g. age (13, 16, 17, 22, ...)
               | ContinuousAttribute { continuousValue :: Double } --e.g. size (1.62, 1.85, ...)
               deriving (Show)
