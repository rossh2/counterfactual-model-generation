module DataStructures where

data Mood = Indicative | Subjunctive | Counterfactual deriving (Eq, Show)
data Time = Past | PastOrPresent | Present | Future | DeltaInDays Int -- Delta from Present, e.g. -1 for yesterday, +1 for tomorrow
    -- in LTL: Past = \exists t < t_0, PastOrPresent = \exists t <= t_0, Present = t_0, Future = \exists t > t_0
    deriving (Eq)

instance Show Time where
    show Past = "Past"
    show PastOrPresent = "Past or Present"
    show Present = "Present"
    show Future = "Future"
    show (DeltaInDays 1) = "Tomorrow"
    show (DeltaInDays (-1)) = "Yesterday"
    show (DeltaInDays n) = if n > 0 then (dayOrDays n) ++ " from today" else (dayOrDays (-n)) ++ " ago"

dayOrDays :: Int -> String
dayOrDays n = if n == 1 then "1 day" else (show n) ++ " days"

data Prop = Prop {
    content :: String
    , negated :: Bool
} deriving (Eq)

instance Show Prop where
    show (Prop content False) = content
    show (Prop content True) = "Not the case that " ++ content

negateProp :: Prop -> Prop
negateProp (Prop content negated) = Prop content (not negated)

data TensedProp = TensedProp { -- TODO is TensedProp the best name for this now that it includes presuppositions? It's a proposition + metadata from the sentence and lexicon
    prop :: Prop
    , time :: Time
    , presuppositions :: [Prop] -- TODO assumes that presuppositions are always present tense / descriptions of current time
} deriving (Show, Eq)

data Conditional = Conditional {
    antecedent :: TensedProp
    , consequent :: TensedProp
    , mood :: Mood
    , timeContrast :: Bool
} deriving (Show, Eq)

data World = World {
    propositions :: [Prop]
    , possible :: Bool -- for the purposes of this LTL-like model the future does NOT count as a possible world
} deriving (Show, Eq)

type WorldTime = (World, Time)
type MinimalModel = [WorldTime] -- TODO would be nice to have this structured in some way this (e.g. sorted/grouped by time), and pretty-printed