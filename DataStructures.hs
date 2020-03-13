module DataStructures where

data Mood = Indicative | Subjunctive | Counterfactual deriving (Eq, Show, Enum)
data Tense = Past | Present | Future
type Time = Integer

data Prop = Prop {
    content :: String
    , negated :: Bool
} deriving (Show, Eq)

data TensedProp = TensedProp {
    prop :: Prop
    , timeDelta :: Time -- TODO more flexible time handling to account for unspecified past, present, future, or specific times e.g. yesterday
    , presuppositions :: [Prop] -- TODO assumes that presuppositions are always present tense / descriptions of current time
    --TODO add a way of telling if repetition of another event
} deriving (Show, Eq)

negateProp :: Prop -> Prop
negateProp (Prop content negated) = Prop content (not negated)

data Conditional = Conditional {
    antecedent :: TensedProp
    , consequent :: TensedProp
    , mood :: Mood
} deriving (Show, Eq)

data World = World {
    propositions :: [Prop]
    , possible :: Bool -- for the purposes of this LTL model the future does NOT count as a possible world
} deriving (Show, Eq)
type WorldTime = (World, Time)
type MinimalModel = [WorldTime] -- TODO would be nice to have this sorted/grouped by time

now = 0  -- TODO would be nice to declare that this has type Time