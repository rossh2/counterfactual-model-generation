module ModelGeneration
    ( generateModel
    , combineModels
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import DataStructures

----------------------
-- MODEL GENERATION --
----------------------

generateModel :: Sentence -> MinimalModel
generateModel (SimpleSentence p) = MinimalModel {
    actualWorlds = Map.fromList [
        generateActualWorldWithProp p
        ]
    , possibleWorlds = Map.empty
    }
generateModel (Conditional p q Indicative False) = MinimalModel { -- timeContrast = False
    actualWorlds = Map.fromList [
        generateActualWorldWithPresupps p
        , generateActualWorldWithConditional p q
        ]
    , possibleWorlds = Map.empty
    }
generateModel (Conditional p q Indicative True) = MinimalModel { -- timeContrast = True
    actualWorlds = Map.fromList [
        -- generateActualWorldWithOppositeOutcome p q -- TODO I don't think we need this - need more data; Starr vaguely supports this
        generateActualWorldWithPresupps p
        , generateActualWorldWithConditional p q
        ]
    , possibleWorlds = Map.empty
    }
generateModel (Conditional p q Subjunctive False) = MinimalModel { -- timeContrast = False
    actualWorlds = Map.fromList [
        generateActualWorldWithPresupps p
        , generateActualWorldWithConditional p q
        ]
    , possibleWorlds = Map.empty
    }
generateModel (Conditional p q Subjunctive True) = MinimalModel { -- timeContrast = True
    actualWorlds = Map.fromList [
        generateActualWorldWithOppositeOutcome p q
        , generateActualWorldWithPresupps p
        , generateActualWorldWithConditional p q
        ]
    , possibleWorlds = Map.empty
    }
generateModel (Conditional p q Counterfactual False) = MinimalModel { -- timeContrast = False
    actualWorlds = Map.fromList [
        generateActualWorldForCounterfactual p q
        ]
    , possibleWorlds = Map.fromList [
        generateCounterfactualWorld p q
        ]
    }
generateModel (Conditional p q Counterfactual True) = MinimalModel { -- timeContrast = True
    actualWorlds = Map.fromList [
        generateActualWorldWithOppositeOutcome p q
        ]
    , possibleWorlds = Map.fromList [
        generateCounterfactualWorld p q
        ]
    }

generateActualWorldWithPresupps :: TensedProp -> (Time, World)
generateActualWorldWithPresupps p = (Present, world)
    where world = World (Set.fromList (presuppositions p)) False

generateActualWorldWithProp :: TensedProp -> (Time, World)
generateActualWorldWithProp p = (time p, world)
    where world = World propsWithPresupps False
          propSet = Set.fromList [prop p]
          propsWithPresupps = addPresuppsToSet p propSet

-- Convenience method, could probably be refactored to use generateActualWorldWithProp
-- TODO n.B. assumes that (time p) == (time q) so can't handle mixed time conditionals
-- such as "If Charlie pays attention in class today, he'll pass the test tomorrow"
-- Could fix this by allowing consequent to return its own world at (time q) if (time q) /= (time p), or always return two worlds and merge if same time
generateActualWorldWithConditional :: TensedProp -> TensedProp -> (Time, World)
generateActualWorldWithConditional p q = (time p, world)
    where world = World propsWithPresupps False
          propSet = Set.fromList [prop p, prop q]
          propsWithPresupps = addPresuppsToSet q (addPresuppsToSet p propSet)

generateActualWorldWithOppositeOutcome :: TensedProp -> TensedProp -> (Time, World)
generateActualWorldWithOppositeOutcome p q = (PastOrPresent, world)
    where world = World propsWithPresupps False
          propSet = Set.fromList [prop p, (negateProp . prop) q]
          -- TODO Assumes that presuppositions of (not p) same as presuppositions of p (not true of special cases like "didn't fail"), see also below
          -- TODO treats (not q) as fact rather than implicature that can be cancelled (also for counterfactual below)
          propsWithPresupps = addPresuppsToSet q (addPresuppsToSet p propSet)

generateActualWorldForCounterfactual :: TensedProp -> TensedProp -> (Time, World)
generateActualWorldForCounterfactual p q = (time p, world)
    where world = World propsWithPresupps False
          propSet = Set.fromList [(negateProp . prop) p, (negateProp . prop) q]
          propsWithPresupps = addPresuppsToSet q (addPresuppsToSet p propSet)

generateCounterfactualWorld :: TensedProp -> TensedProp -> (Time, World)
generateCounterfactualWorld p q = (time p, world)
    where world = World propsWithPresupps True
          propSet = Set.fromList [prop p, prop q]
          propsWithPresupps = addPresuppsToSet q (addPresuppsToSet p propSet)

addPresuppsToSet :: TensedProp -> Set.Set Prop -> Set.Set Prop
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
-- If (not q) is not in the set already, it's safe to add q, otherwise return Nothing
addProp q (Just ps) = if Set.notMember (Prop (content q) ((not . negated) q)) ps then Just (Set.insert q ps) else Nothing
