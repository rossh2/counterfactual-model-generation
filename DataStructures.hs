module DataStructures where

import Data.Ord
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Time

data Mood = Indicative | Subjunctive | Counterfactual deriving (Eq, Show)

data Prop = Prop {
    content :: String
    , negated :: Bool
    , cancellable :: Bool
} deriving (Eq, Ord)
propWithDefault = Prop "" False False

instance Show Prop where
    show (Prop content False cancellable) = if cancellable then content ++ " (implicature)" else content
    show (Prop content True cancellable) = if cancellable then negatedContent ++ " (implicature)" else negatedContent
        -- In principle, sometimes the first word of `content` should get lowercased (if not a proper noun)
        where negatedContent = "Not the case that " ++ content

negateProp :: Prop -> Prop
negateProp (Prop content negated cancellable) = Prop content (not negated) cancellable

implicature :: Prop -> Prop
implicature (Prop content negated cancellable) = Prop content negated True

data ParsedProp = ParsedProp {
    prop :: Prop
    , time :: Time.Time
    , presuppositions :: [Prop] -- TODO assumes that presuppositions are always present tense / descriptions of time at which proposition holds
} deriving (Show, Eq)

data Sentence = SimpleSentence {
        tprop :: ParsedProp
    } | Conditional {
        antecedent :: ParsedProp
        , consequent :: ParsedProp
        , mood :: Mood
        , timeContrast :: Bool
    } deriving (Show, Eq)

type Discourse = [Sentence]

data World = World {
    propositions :: Set.Set Prop
    , possible :: Bool -- for the purposes of this LTL-like model the future does NOT count as a possible world
} deriving (Eq)

instance Show World where
    show w = "World {" ++ (show . Set.toList . propositions) w ++ ", " ++ poss ++ "}"
        where poss = if possible w then "possible" else "actual"

data MinimalModel = MinimalModel {
    actualWorlds :: Map.Map Time.Time World
    , possibleWorlds :: Map.Map Time.Time World
} deriving (Eq)

instance Show MinimalModel where
    show m = "MinimalModel {\nActual worlds: " ++ showActual ++ "\nPossible worlds: " ++ showPossible ++ "}"
        where showActual = (show . Map.toList . actualWorlds) m
              showPossible = (show . Map.toList . possibleWorlds) m
