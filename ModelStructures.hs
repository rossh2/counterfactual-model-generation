module ModelStructures where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import DataStructures
import Times
import TypeClasses

data World = World {
    propositions :: Set.Set Prop
    , possible :: Bool -- for the purposes of this LTL-like model the future does NOT count as a possible world
} deriving (Eq, Show)

instance ShowLinear World where
    showLin w = (showLinList . Set.toList . propositions) w

data MinimalModel = MinimalModel {
    actualWorlds :: Map.Map Time World
    , possibleWorlds :: Map.Map Time World
} deriving (Eq, Show)

instance ShowLinear MinimalModel where
    showLin m = "Actual worlds: " ++ showActual ++ "\nPossible worlds: " ++ showPossible
        where showActual = (showLinList . Map.toList . actualWorlds) m
              showPossible = (showLinList . Map.toList . possibleWorlds) m
