module Data where

data Prop = Prop {
    content :: String
    , negated :: Bool
} deriving (Show, Eq)

data Mood = Indicative | Subjunctive | Counterfactual deriving (Eq, Show, Enum)

data Conditional = Conditional {
    antecedent :: Prop
    , consequent :: Prop
    , mood :: Mood
} deriving (Show, Eq)

data World = World { propositions :: [Prop], possible :: Bool } deriving (Show, Eq)

type Time = Integer
type WorldTime = (World, Time)

now = 0  -- TODO would be nice to declare that this has type Time