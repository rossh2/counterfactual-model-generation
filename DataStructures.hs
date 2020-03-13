module DataStructures where

data Mood = Indicative | Subjunctive | Counterfactual deriving (Eq, Show)
data Time = Past | Present | PastOrPresent | Future | Yesterday | Tomorrow -- TODO extend with arbitrary other times ideally
    deriving (Eq, Show)

data Prop = Prop {
    content :: String
    , negated :: Bool
} deriving (Eq)

instance Show Prop where
  show (Prop content False) = show content
  show (Prop content True) = show ("Not the case that " ++ content)

data TensedProp = TensedProp {
    prop :: Prop
    , time :: Time
    , presuppositions :: [Prop] -- TODO assumes that presuppositions are always present tense / descriptions of current time
} deriving (Show, Eq)

negateProp :: Prop -> Prop
negateProp (Prop content negated) = Prop content (not negated)

data Conditional = Conditional {
    antecedent :: TensedProp
    , consequent :: TensedProp
    , mood :: Mood
    , timeFocused :: Bool
} deriving (Show, Eq)

data World = World {
    propositions :: [Prop]
    , possible :: Bool -- for the purposes of this LTL-like model the future does NOT count as a possible world
} deriving (Show, Eq)
type WorldTime = (World, Time)
type MinimalModel = [WorldTime] -- TODO would be nice to have this sorted/grouped by time