module Parsing.Parsing
    ( parseSentence
    , parseDiscourse
    ) where

import qualified Data.List as List

import Grammar.Features
import Grammar.Grammar
import Grammar.Lexicon
import Model.Times
import Parsing.DataStructures
import Utils.TypeClasses

parseDiscourse :: [SentTree] -> Maybe ParsedDiscourse
parseDiscourse [] = Just []
parseDiscourse (tree:xs) = case parsedSentence of
                                Just pSentence -> case parseRest of
                                    Just pRest -> Just (pSentence : pRest)
                                    Nothing -> Nothing
                                Nothing -> Nothing
    where parsedSentence = parseSentence tree
          parseRest = parseDiscourse xs

parseSentence :: SentTree -> Maybe ParsedSentence
parseSentence tree@(Sent1 tp)  = case parsedProp of
                                    Just pProp -> Just (ParsedSimpleSentence pProp)
                                    Nothing -> Nothing
    where parsedProp = parseSimpleSentence tree tp
parseSentence tree@(Sent2 c tpP tpQ) = if c == ifC
                                          then case parsedP of
                                              Just pP -> case fixedQ of
                                                  Just fQ -> Just (ParsedConditional {
                                                      antecedent = pP
                                                      , consequent = fQ
                                                      , fullTree = tree
                                                      })
                                                  Nothing -> Nothing
                                              Nothing -> Nothing
                                       else Nothing -- Other C heads not implemented
    where parsedP = parseSimpleSentence (Sent1 tpP) tpP
          parsedQ = parseSimpleSentence (Sent1 tpQ) tpQ
          fixedQ = inferConsequentTime parsedP parsedQ

parseSimpleSentence :: SentTree -> TP -> Maybe ParsedProp
parseSimpleSentence tree tp@(TP np vp) = case parsedProp of
                                   Just pProp -> case parsedMood of
                                       Just pMood -> Just (ParsedProp {
                                           prop = pProp
                                           , mood = pMood
                                           , presuppositions = (parsePresupps tp)
                                           , tree = tree
                                           })
                                       Nothing -> Nothing
                                   Nothing -> Nothing
    where parsedProp = parseProp tp
          parsedMood = parseMood vp

parseProp :: TP -> Maybe Prop
parseProp tp@(TP np vp) = case parsedEvent of
                            Just pEvent -> Just (Prop {
                                event = pEvent
                                , negated = isVPNegated vp
                                , cancellable = False
                                })
                            Nothing -> Nothing
    where parsedEvent = parseEvent tp

parseEvent :: TP -> Maybe Event
parseEvent (TP np vp) = case parsedTime of
                            Just pTime -> Just (Event {
                                agent = showLin np
                                , predicate = vRoot rootV
                                , arguments = parseArguments vp
                                , time = pTime
                                , predProps = predicateProps rootV
                                })
                            Nothing -> Nothing
    where rootV = getRootV vp
          parsedTime = parseTime vp

parseArguments :: VP -> [String]
parseArguments (VP1 v) = []
parseArguments (VP2 v np) = [showLin np]
parseArguments (VP3 neg vp) = parseArguments vp
parseArguments (VP4 aux vp) = parseArguments vp
parseArguments (VP5 vp adj) = parseArguments vp

parseTime :: VP -> Maybe Time
-- Assume temporal adverb supersedes tense, don't validate if they match (they won't anyway for mismatched past counterfactuals)
-- Enforce that there is at most one temporal adverbial, otherwise fail to parse and return Nothing
parseTime vp = if not (null adjunctTimes) then
                    if length adjunctTimes == 1 then head adjunctTimes else Nothing
               else tenseTime
    where tenseTime = verbFormsToTime (getAuxEffects vp) (getVerbForms vp)
          adjunctTimes = filter (\x -> x /= Nothing) $ map timeFromAdjunct $ getAdjuncts vp

parseMood :: VP -> Maybe Mood
parseMood vp = auxEffectsToMood (getAuxEffects vp)

parsePresupps :: TP -> [Prop]
parsePresupps (TP np vp) = [] -- TODO implement presupposition parsing

-- This is a heuristic: will be incorrect for mixed time conditionals such as
-- "If he had gone shopping yesterday, then he would still have food (now/in the future)."
inferConsequentTime :: Maybe ParsedProp -> Maybe ParsedProp -> Maybe ParsedProp
inferConsequentTime p Nothing = Nothing -- No consequent, nothing to fix
inferConsequentTime Nothing q = q -- No antecedent, no information to fix with
inferConsequentTime (Just p) (Just q) = if antecedentMoreSpecific
                                        then Just (applyTimeToProp (parsedPropTime p) q)
                                      else Just q
    where antecedentMoreSpecific = ((isSpecificTime . parsedPropTime) p && (not . isSpecificTime . parsedPropTime) q)
                                   || (parsedPropTime q) == Unknown -- This is the least specific, anything is better than this

applyTimeToProp :: Time -> ParsedProp -> ParsedProp
applyTimeToProp t q = q { prop = newProp }
    where newEvent = ((event . prop) q) { time = t}
          newProp = (prop q) { event = newEvent }
