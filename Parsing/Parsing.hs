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
                                              Just pP -> case parsedQ of
                                                  Just pQ -> Just (ParsedConditional {
                                                      antecedent = pP
                                                      , consequent = pQ
                                                      , fullTree = tree
                                                      })
                                                  Nothing -> Nothing
                                              Nothing -> Nothing
                                       else Nothing -- Other C heads not implemented
    where parsedP = parseSimpleSentence (Sent1 tpP) tpP
          parsedQ = parseSimpleSentence (Sent1 tpQ) tpQ

parseSimpleSentence :: SentTree -> TP -> Maybe ParsedProp
parseSimpleSentence tree tp@(TP np vp) = case parsedTime of
                                   Just pTime -> case parsedMood of
                                       Just pMood -> Just (ParsedProp {
                                           prop = parseProp tp
                                           , time = pTime
                                           , mood = pMood
                                           , presuppositions = (parsePresupps tp)
                                           , tree = tree
                                           })
                                       Nothing -> Nothing
                                   Nothing -> Nothing
    where parsedTime = parseTime vp
          parsedMood = parseMood vp

parseProp :: TP -> Prop
parseProp tp@(TP np vp) = Prop {
    content = parseEvent tp
    , negated = isVPNegated vp
    , cancellable = False
    }

parseEvent :: TP -> Event
parseEvent (TP np vp) = Event {
        agent = showLin np
        , predicate = vRoot rootV
        , arguments = parseArguments vp
        , predProps = predicateProps rootV
        }
    where rootV = getRootV vp

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

-- TODO Is it better to set the consequent's time to be the time of the antecedent, or to keep it as unknown?
-- Not relevant for MinimalModel right now anyway, as time of whole conditional is taken from antecedent
-- (Currently this method is not used)
fixConsequentTime :: Maybe ParsedProp -> Maybe ParsedProp -> Maybe ParsedProp
fixConsequentTime p Nothing = Nothing -- No consequent, nothing to fix
fixConsequentTime Nothing q = q -- No antecedent, no information to fix with
fixConsequentTime (Just p) (Just q) = if time q == PastOrPresent -- use to represent unknown time
                                        then Just (q { time = time p })
                                      else Just q
