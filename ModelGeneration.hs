module ModelGeneration
    ( generateModel
    ) where

import DataStructures

generateModel :: Conditional -> [WorldTime]
generateModel (Conditional p q Indicative) = [
    (World [] False, now),
    (World [prop p, prop q] False, getTime p now)
    ]
generateModel (Conditional p q Subjunctive) = [
    (World [] False, now),
    (World [prop p, prop q] False, getTime p now)
    ]
generateModel (Conditional p q Counterfactual) = [
    (World [(negateProp . prop) p, (negateProp . prop) q] False, now),
    (World [prop p, prop q] True, getTime p now)
    ]

getTime :: TensedProp -> Time -> Time
getTime p t = (timeDelta p) + t
