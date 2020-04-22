module Model.ModelGeneration
    ( generateSentenceModel
    , generateDiscourseModel
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Grammar.Features
import Grammar.Semantics
import Model.ModelStructures
import Model.Times
import Parsing.DataStructures

----------------------
-- MODEL GENERATION --
----------------------

emptyModel = Just (MinimalModel { actualWorlds = Map.empty, possibleWorlds = Map.empty })

generateDiscourseModel :: ParsedDiscourse -> Maybe MinimalModel
generateDiscourseModel sents = foldl addSentenceToModel emptyModel sents

-- Convenience method, will in fact always return a model
generateSentenceModel :: ParsedSentence -> Maybe MinimalModel
generateSentenceModel s = addSentenceToModel emptyModel s

addSentenceToModel :: Maybe MinimalModel -> ParsedSentence -> Maybe MinimalModel
addSentenceToModel m s@(ParsedSimpleSentence p) = combineModels (Just (generateSimpleModel s)) m
addSentenceToModel m s@(ParsedConditional p q sentTree) = combineModels (Just model) m
    where timeContrast = checkTimeContrast s m
          model = if timeContrast then generateTimeContrastModel s else generateSimpleModel s

generateSimpleModel :: ParsedSentence -> MinimalModel
generateSimpleModel (ParsedSimpleSentence p) = MinimalModel {
    actualWorlds = worldsFromProps False $ propWithPresupps p
    , possibleWorlds = Map.empty
    }
generateSimpleModel (ParsedConditional p q sentTree) = case conditionalMood of
        Indicative -> MinimalModel {
            actualWorlds = worldsFromProps False $
                makePresuppositionsAtTime Present p
                ++ conditionalWithPresupps p q
            , possibleWorlds = Map.empty
            }
        Subjunctive -> MinimalModel {
            actualWorlds = worldsFromProps False $
                makePresuppositionsAtTime Present p
                ++ conditionalWithPresupps p q
            , possibleWorlds = Map.empty
            }
        Counterfactual -> MinimalModel {
            actualWorlds = worldsFromProps False $
                oppositePastConditionalWithPresupps p q
            , possibleWorlds = worldsFromProps True $
                conditionalWithPresupps p q
            }
    where conditionalMood = mood q

generateTimeContrastModel :: ParsedSentence -> MinimalModel
generateTimeContrastModel s@(ParsedSimpleSentence p) = generateSimpleModel s -- Simple sentences don't have time contrast behaviour
generateTimeContrastModel (ParsedConditional p q sentTree) = case conditionalMood of
        Indicative -> MinimalModel {
            actualWorlds = worldsFromProps False $
                makePresuppositionsAtTime Present p
                ++ conditionalWithPresupps p q
            , possibleWorlds = Map.empty
            }
        Subjunctive -> MinimalModel {
            actualWorlds = worldsFromProps False $
                makePresuppositionsAtTime Present p
                ++ conditionalWithPresupps p q
                ++ oppositeOutcomePastConditionalWithPresupps p q
            , possibleWorlds = Map.empty
            }
        Counterfactual -> MinimalModel {
            actualWorlds = worldsFromProps False $
                oppositePastConditionalWithPresupps p q
            , possibleWorlds = worldsFromProps True $
                conditionalWithPresupps p q
            }
    where conditionalMood = mood q

-- It's important that you supply all the props you intend to add at once, since (only) this does proper merging on times
worldsFromProps :: Bool -> [Prop] -> Map.Map Time World
worldsFromProps possible ps = foldr (addPropToWorlds possible) Map.empty ps

-- n.B. no sanity check is done that the worlds have the same possible value as the one being passed
addPropToWorlds :: Bool -> Prop -> Map.Map Time World -> Map.Map Time World
addPropToWorlds possible p map = if timeExists then Map.adjust (updateWorld p) pTime map
                                 else Map.insert pTime newWorld map
    where pTime = (time . event) p
          timeExists = Map.member pTime map
          newWorld = World { propositions = Set.singleton p, possible = possible }

updateWorld :: Prop -> World -> World
updateWorld p w = w { propositions = newPropositions }
    where newPropositions = Set.insert p (propositions w)

propWithPresupps :: ParsedProp -> [Prop]
propWithPresupps p = prop p : (presuppositions p)

conditionalWithPresupps :: ParsedProp -> ParsedProp -> [Prop]
conditionalWithPresupps p q = [prop p, prop q] ++ (presuppositions p) ++ (presuppositions q)

oppositeOutcomePastConditionalWithPresupps :: ParsedProp -> ParsedProp -> [Prop]
oppositeOutcomePastConditionalWithPresupps p q = [setPropTime Past (prop p), setPropTime Past $ makeOppositeOutcomeProp q]
                                            ++ (makePresuppositionsAtTime Past p)
                                            ++ (makePresuppositionsAtTime Past q)

oppositePastConditionalWithPresupps :: ParsedProp -> ParsedProp -> [Prop]
oppositePastConditionalWithPresupps p q = [setPropTime Past $ makeOppositeOutcomeProp p, setPropTime Past $ makeOppositeOutcomeProp q]
                                            ++ (makePresuppositionsAtTime Past p)
                                            ++ (makePresuppositionsAtTime Past q)

makePresuppositionsAtTime :: Time -> ParsedProp -> [Prop]
makePresuppositionsAtTime time p = map (setPropTime time) (presuppositions p)

makeOppositeOutcomeProp :: ParsedProp -> Prop
makeOppositeOutcomeProp p = (implicature . negateProp . prop) p

setPropTime :: Time -> Prop -> Prop
setPropTime time p = p { event = pastEvent }
    where pastEvent = (event p) { time = time }

-----------------------
-- MODEL COMBINATION --
-----------------------

-- Could rewrite this as "combineModelsNoMaybe <$> model1 <*> model2" but it's probably not worth it
combineModels :: Maybe MinimalModel -> Maybe MinimalModel -> Maybe MinimalModel
combineModels Nothing m = Nothing
combineModels m Nothing = Nothing
combineModels (Just m) (Just n) = case actualCombined of
                                      Just aCombined -> case possibleCombined of
                                          Just pCombined -> Just (MinimalModel aCombined pCombined)
                                          Nothing -> Nothing
                                      Nothing -> Nothing
    where actualCombined = combineWorldsByTime (actualWorlds m) (actualWorlds n)
          possibleCombined = combineWorldsByTime (possibleWorlds m) (possibleWorlds n)

combineWorldsByTime :: Map.Map Time World -> Map.Map Time World -> Maybe (Map.Map Time World)
combineWorldsByTime map1 map2 = foldl insertCombined (Just Map.empty) allTimes
      where allTimes = Set.union (Map.keysSet map1) (Map.keysSet map2)
            combineWorldsAtT = \t -> combineWorlds (Map.lookup t map1) (Map.lookup t map2)
            -- Add result of combining the worlds to combined map unless that returns Nothing, in which case make the whole map Nothing
            insertCombined = \map t -> (Map.insert $ t) <$> (combineWorldsAtT t) <*> map

combineWorlds :: Maybe World -> Maybe World -> Maybe World
combineWorlds Nothing  Nothing  = Nothing
combineWorlds Nothing  (Just v) = Just v
combineWorlds (Just w) Nothing  = Just w
combineWorlds (Just w) (Just v) = if possible w /= possible v then Nothing else combined -- Sanity check that worlds can be combined
    where allProps = foldr addProp (Just (propositions w)) (propositions v)
          combined = case allProps of Just ps -> Just (World ps (possible w))
                                      Nothing -> Nothing

-- Depends on, but does not check that the existing set of props is consistent
-- (i.e. at most one prop describes the same event)
addProp :: Prop -> Maybe (Set.Set Prop) -> Maybe (Set.Set Prop)
addProp q Nothing = Nothing
addProp q (Just ps) = if safeToAdd then Just (Set.insert q ps)
                      else if cancelExisting then Just (Set.insert q newPs)
                      else if nothingToDo then Just ps
                      else Nothing
    where matching = Set.filter (\p -> event p == event q) ps
          antonymMatching = Set.filter (\p -> antonymEvent p q) ps
          antonymSameMeaning = if not (null antonymMatching) then negated (Set.elemAt 0 antonymMatching) /= negated q else False
          safeToAdd = null matching && null antonymMatching
          -- Can cancel only if the the meaning is the same, or if the same predicate is used (not the antonym)
          safeToCancel = not (null matching) || antonymSameMeaning
          matchedPred = if not (null matching) then Set.elemAt 0 matching
                        else if not (null antonymMatching) then Set.elemAt 0 antonymMatching
                        else q -- Dummy value
          cancelExisting = if safeToCancel then cancellable matchedPred else False
          nothingToDo = if safeToCancel then cancellable q || matchedPred == q || antonymSameMeaning else False
          newPs = if cancelExisting then Set.delete (Set.elemAt 0 matching) ps else ps

antonymEvent :: Prop -> Prop -> Bool
antonymEvent p q = case pAntonymPred of Just pAPred -> pEvent { predicate = pAPred } == qEvent
                                        Nothing     -> False
    where pEvent = event p
          qEvent = event q
          pAntonymPred = (antonymPred . predicate) pEvent


--------------------------------------
-- TIME CONTRAST / EVENT REPETITION --
--------------------------------------

checkTimeContrast :: ParsedSentence -> Maybe MinimalModel -> Bool
checkTimeContrast _ Nothing = False
checkTimeContrast (ParsedSimpleSentence p) (Just m) = eventRepetition || predicateRepetition
    where eventRepetition = isRepeatedEvent p m
          predicateRepetition = Repetition `elem` (predProps . event . prop) p
checkTimeContrast (ParsedConditional p q sentTree) (Just m) = conditionalMood == Counterfactual || predicateRepetition || eventRepetition
    where conditionalMood = mood q
          predicateRepetition = Repetition `elem` (predProps . event . prop) p
          eventRepetition = isRepeatedEvent p m

-- TODO unclear whether this is needed - counterfactual + predicate repetition flag on re-take actually handles all our cases right now
-- But this is probably needed for the ice-cream case which is naturally repeatable without use of "re-get ice-cream"
isRepeatedEvent :: ParsedProp -> MinimalModel -> Bool
isRepeatedEvent pProp m = Set.member pEvent modelActualEvents
    where pEvent = (event . prop) pProp
          modelActualProps = Set.unions $ Map.map propositions (actualWorlds m)
          modelActualEvents = Set.map event modelActualProps

