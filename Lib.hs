module Lib
    ( generateModel
    ) where

import Data

someFunc :: IO ()
someFunc = putStrLn "someFunc"

generateModel :: Conditional -> [WorldTime]
generateModel (Conditional p q Indicative) = [(World [] False, now), (World [p, q] True, now)]
generateModel (Conditional p q Subjunctive) = [(World [] False, now), (World [p, q] True, now)]
generateModel (Conditional p q Counterfactual) = [(World [negateProp p, negateProp q] False, now), (World [p, q] True, now)]

negateProp :: Prop -> Prop
negateProp (Prop content negated) = Prop content (not negated)