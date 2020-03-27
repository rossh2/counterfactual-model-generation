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
    content :: Event
    , negated :: Bool
    , cancellable :: Bool
} deriving (Eq, Ord, Show)
propWithDefaults e = Prop e False False

instance ShowLinear Prop where
    showLin (Prop content False cancellable) = if cancellable then showContent ++ " (implicature)" else showContent
        where showContent = showLin content
    showLin (Prop content True cancellable) = if cancellable then negatedContent ++ " (implicature)" else negatedContent
        -- In principle, sometimes the first word of `content` should get lowercased (if not a proper noun)
        where negatedContent = "Not the case that " ++ (showLin content)

negateProp :: Prop -> Prop
negateProp (Prop content negated cancellable) = Prop content (not negated) cancellable

implicature :: Prop -> Prop
implicature (Prop content negated cancellable) = Prop content negated True

data ParsedProp = ParsedProp {
    prop :: Prop
    , mood :: Mood
    , presuppositions :: [Prop]
    , tree :: SentTree -- Attached for e.g. showing original sentence
} deriving (Show, Eq)

parsedPropTime :: ParsedProp -> Time
parsedPropTime = time . content . prop

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
