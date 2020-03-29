module Parsing.DataStructures where

import Data.Ord
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Grammar.Features
import Grammar.Grammar
import Grammar.Lexicon
import Model.Times
import Utils.TypeClasses

-- Rough implementation of Davidsonian events, with some extras (predicate properties)
-- TODO in the long term, event components shouldn't be strings but of e.g. lambda-expressions for meaning
data Event = Event {
    agent :: String
    , predicate :: String
    , arguments :: [String]
    , time :: Time
    , predProps :: [PredicateProps]
} deriving (Eq, Ord, Show)

instance ShowLinear Event where
    -- This is a bit rough and ready as it uses the verb root when parsed, which does not agree with 3Sg
    showLin e = agent e ++ " " ++ predicate e ++ " " ++ showArguments
        where showArguments = (unwords . arguments) e

data Prop = Prop {
    event :: Event -- TODO this should be called 'event'
    , negated :: Bool
    , cancellable :: Bool
} deriving (Eq, Ord, Show)
propWithDefaults e = Prop e False False

instance ShowLinear Prop where
    showLin (Prop event False cancellable) = if cancellable then showEvent ++ " (implicature)" else showEvent
        where showEvent = showLin event
    showLin (Prop event True cancellable) = if cancellable then negatedEvent ++ " (implicature)" else negatedEvent
        -- In principle, sometimes the first word of `event` should get lowercased (if not a proper noun)
        where negatedEvent = "Not the case that " ++ (showLin event)

negateProp :: Prop -> Prop
negateProp (Prop event negated cancellable) = Prop event (not negated) cancellable

implicature :: Prop -> Prop
implicature (Prop event negated cancellable) = Prop event negated True

data ParsedProp = ParsedProp {
    prop :: Prop
    , mood :: Mood
    , presuppositions :: [Prop]
    , tree :: SentTree -- Attached for e.g. showing original sentence
} deriving (Show, Eq)

parsedPropTime :: ParsedProp -> Time
parsedPropTime = time . event . prop

data ParsedSentence = ParsedSimpleSentence {
        pProp :: ParsedProp
    } | ParsedConditional {
        antecedent :: ParsedProp
        , consequent :: ParsedProp
        , fullTree :: SentTree
    } deriving (Show, Eq)

instance ShowLinear ParsedSentence where
    showLin (ParsedSimpleSentence p) = (showLin . tree) p
    showLin (ParsedConditional p q sentTree) = showLin sentTree
    showLinList [] = ""
    showLinList (x:xs) = showLin x ++ showl xs
        where showl [] = ""
              showl (x:xs) = " " ++ showLin x ++ showl xs


type ParsedDiscourse = [ParsedSentence]
