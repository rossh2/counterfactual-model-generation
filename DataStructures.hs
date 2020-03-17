module DataStructures where

import Data.Ord
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

data Mood = Indicative | Subjunctive | Counterfactual deriving (Eq, Show)
data Time = Past | PastOrPresent | Present | Future | DeltaInDays Int -- Delta from Present, e.g. -1 for yesterday, +1 for tomorrow
    -- in LTL: Past = \exists t < t_0, PastOrPresent = \exists t <= t_0, Present = t_0, Future = \exists t > t_0
    deriving (Eq) -- Ord, Show declared below

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

instance Ord Time where
    (<=) Past            t               = True
    -- It's not clear where PastOrPresent should sit, but for the purpose of this ordering imagine that it sits a tiny delta of time before the present
    -- So any definitely past time is earlier than it, but the (definitely) present is later than it, and no definite time is equal to it
    (<=) PastOrPresent   Past            = False
    (<=) PastOrPresent   PastOrPresent   = True
    (<=) PastOrPresent   Present         = True
    (<=) PastOrPresent   Future          = True
    (<=) PastOrPresent   (DeltaInDays n) = n >= 0
    (<=) Present         Past            = False
    (<=) Present         PastOrPresent   = False
    (<=) Present         Present         = True
    (<=) Present         Future          = True
    (<=) Present         (DeltaInDays n) = n >= 0
    (<=) Future          Future          = True
    (<=) Future          t               = False
    (<=) (DeltaInDays n) Past            = False
    (<=) (DeltaInDays n) PastOrPresent   = n < 0
    (<=) (DeltaInDays n) Present         = n <= 0
    (<=) (DeltaInDays n) Future          = True

data Prop = Prop {
    content :: String
    , negated :: Bool
} deriving (Eq, Ord)

instance Show Prop where
    show (Prop content False) = content
    -- In principle, sometimes the first word of `content` should get lowercased (if not a proper noun)
    show (Prop content True) = "Not the case that " ++ content

negateProp :: Prop -> Prop
negateProp (Prop content negated) = Prop content (not negated)

-- TODO is TensedProp the best name for this now that it includes presuppositions? It's a proposition + metadata from the sentence and lexicon
data TensedProp = TensedProp {
    prop :: Prop
    , time :: Time
    , presuppositions :: [Prop] -- TODO assumes that presuppositions are always present tense / descriptions of current time
} deriving (Show, Eq)

data Sentence = SimpleSentence {
        tprop :: TensedProp
    } | Conditional {
        antecedent :: TensedProp
        , consequent :: TensedProp
        , mood :: Mood
        , timeContrast :: Bool
    } deriving (Show, Eq)

data World = World {
    propositions :: Set.Set Prop
    , possible :: Bool -- for the purposes of this LTL-like model the future does NOT count as a possible world
} deriving (Eq)

instance Show World where
    show w = "World {" ++ (show . Set.toList . propositions) w ++ ", " ++ poss ++ "}"
        where poss = if possible w then "possible" else "actual"

data MinimalModel = MinimalModel {
    actualWorlds :: Map.Map Time World
    , possibleWorlds :: Map.Map Time World
} deriving (Eq)

instance Show MinimalModel where
    show m = "MinimalModel {\nActual worlds: " ++ showActual ++ "\nPossible worlds: " ++ showPossible ++ "}"
        where showActual = (show . Map.toList . actualWorlds) m
              showPossible = (show . Map.toList . possibleWorlds) m