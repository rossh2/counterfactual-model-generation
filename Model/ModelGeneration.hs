module Model.ModelGeneration
    ( generateModel
    , generateDiscourseModel
    , combineModels
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Grammar.Features
import Model.ModelStructures
import Model.Times
import Parsing.DataStructures

----------------------
-- MODEL GENERATION --
----------------------

generateDiscourseModel :: ParsedDiscourse -> Maybe MinimalModel
generateDiscourseModel [] = Just (MinimalModel { actualWorlds = Map.empty, possibleWorlds = Map.empty })
generateDiscourseModel (x:xs) = combineModels (Just (generateModel x)) (generateDiscourseModel xs)

generateModel :: ParsedSentence -> MinimalModel
generateModel (ParsedSimpleSentence p) = MinimalModel {
    actualWorlds = Map.fromList [
        generateActualWorldWithProp p
        ]
    , possibleWorlds = Map.empty
    }
generateModel (ParsedConditional p q False sentTree) = case conditionalMood of -- timeContrast = False
        Indicative -> MinimalModel {
            actualWorlds = Map.fromList [
                generateActualWorldWithPresupps p
                , generateActualWorldWithConditional p q
                ]
            , possibleWorlds = Map.empty
            }
        Subjunctive -> MinimalModel {
            actualWorlds = Map.fromList [
                generateActualWorldWithPresupps p
                , generateActualWorldWithConditional p q
                ]
            , possibleWorlds = Map.empty
            }
        Counterfactual -> MinimalModel {
            actualWorlds = Map.fromList [
                generateActualWorldForCounterfactual p q
                ]
            , possibleWorlds = Map.fromList [
                generateCounterfactualWorld p q
                ]
            }
    where conditionalMood = mood q
generateModel (ParsedConditional p q True sentTree) = case conditionalMood of -- timeContrast = True
        Indicative -> MinimalModel { -- timeContrast = True
            actualWorlds = Map.fromList [
                -- generateActualWorldWithOppositeOutcome p q -- TODO I don't think we need this - need more data; Starr vaguely supports this
                generateActualWorldWithPresupps p
                , generateActualWorldWithConditional p q
                ]
            , possibleWorlds = Map.empty
            }
        Subjunctive -> MinimalModel { -- timeContrast = True
            actualWorlds = Map.fromList [
                generateActualWorldWithOppositeOutcome p q
                , generateActualWorldWithPresupps p
                , generateActualWorldWithConditional p q
                ]
            , possibleWorlds = Map.empty
            }
        Counterfactual -> MinimalModel { -- timeContrast = True
            actualWorlds = Map.fromList [
                generateActualWorldWithOppositeOutcome p q
                ]
            , possibleWorlds = Map.fromList [
                generateCounterfactualWorld p q
                ]
            }
    where conditionalMood = mood q


generateActualWorldWithPresupps :: ParsedProp -> (Time, World)
generateActualWorldWithPresupps p = (Present, world)
    where world = World (Set.fromList (presuppositions p)) False

generateActualWorldWithProp :: ParsedProp -> (Time, World)
generateActualWorldWithProp p = (time p, world)
    where world = World propsWithPresupps False
          propSet = Set.fromList [prop p]
          propsWithPresupps = addPresuppsToSet p propSet

-- Convenience method, could probably be refactored to use generateActualWorldWithProp
-- TODO Assumes that (time p) == (time q) so can't handle mixed time conditionals
-- such as "If Charlie pays attention in class today, he'll pass the test tomorrow"
-- Could fix this by allowing consequent to return its own world at (time q) if (time q) /= (time p), or always return two worlds and merge if same time
generateActualWorldWithConditional :: ParsedProp -> ParsedProp -> (Time, World)
generateActualWorldWithConditional p q = (time p, world)
    where world = World propsWithPresupps False
          propSet = Set.fromList [prop p, prop q]
          propsWithPresupps = addPresuppsToSet q (addPresuppsToSet p propSet)

generateActualWorldWithOppositeOutcome :: ParsedProp -> ParsedProp -> (Time, World)
generateActualWorldWithOppositeOutcome p q = (Past, world)
    where world = World propsWithPresupps False
          propSet = Set.fromList [prop p, (implicature . negateProp . prop) q]
          -- TODO Assumes that presuppositions of (not p) same as presuppositions of p (not true of special cases like "didn't fail"), see also below
          propsWithPresupps = addPresuppsToSet q (addPresuppsToSet p propSet)

generateActualWorldForCounterfactual :: ParsedProp -> ParsedProp -> (Time, World)
generateActualWorldForCounterfactual p q = (time p, world)
    where world = World propsWithPresupps False
          propSet = Set.fromList [(implicature . negateProp . prop) p, (implicature . negateProp . prop) q]
          propsWithPresupps = addPresuppsToSet q (addPresuppsToSet p propSet)

generateCounterfactualWorld :: ParsedProp -> ParsedProp -> (Time, World)
generateCounterfactualWorld p q = (time p, world)
    where world = World propsWithPresupps True
          propSet = Set.fromList [prop p, prop q]
          propsWithPresupps = addPresuppsToSet q (addPresuppsToSet p propSet)

addPresuppsToSet :: ParsedProp -> Set.Set Prop -> Set.Set Prop
addPresuppsToSet p props = foldr Set.insert props (presuppositions p)

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

addProp :: Prop -> Maybe (Set.Set Prop) -> Maybe (Set.Set Prop)
addProp q Nothing = Nothing
addProp q (Just ps) = if safeToAdd then Just (Set.insert q ps)
                      else if cancelExisting then Just (Set.insert q newPs)
                      else if nothingToDo then Just ps
                      else Nothing
    where matching = Set.filter (\p -> content p == content q) ps
          --inconsistent = Set.size matching > 1 -- If the set of props is consistent (built by this method), there is at most one match.
          safeToAdd = null matching
          cancelExisting = if not safeToAdd then cancellable (Set.elemAt 0 matching) else False -- && not inconsistent
          nothingToDo = if not safeToAdd then (Set.elemAt 0 matching) == q || (cancellable q && not cancelExisting) else False
          newPs = if cancelExisting then Set.delete (Set.elemAt 0 matching) ps else ps
