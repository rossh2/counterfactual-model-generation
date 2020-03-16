module ModelGeneration
    ( generateModel
    , validateTime
    ) where

import DataStructures

generateModel :: Conditional -> MinimalModel
generateModel (Conditional p q Indicative False) = [ -- timeContrast = False
    generateActualWorldWithPresupps p,
    generateActualWorldWithConditional p q
    ]
generateModel (Conditional p q Indicative True) = [ -- timeContrast = True
    -- generateActualWorldWithOppositeOutcome p q, -- TODO I don't think we need this - need more data; Starr vaguely supports this
    generateActualWorldWithPresupps p,
    generateActualWorldWithConditional p q
    ]
generateModel (Conditional p q Subjunctive False) = [ -- timeContrast = False
    generateActualWorldWithPresupps p,
    generateActualWorldWithConditional p q
    ]
generateModel (Conditional p q Subjunctive True) = [ -- timeContrast = True
    generateActualWorldWithOppositeOutcome p q,
    generateActualWorldWithPresupps p,
    generateActualWorldWithConditional p q
    ]
generateModel (Conditional p q Counterfactual False) = [ -- timeContrast = False
    generateActualWorldForCounterfactual p q,
    generateCounterfactualWorld p q
    ]
generateModel (Conditional p q Counterfactual True) = [ -- timeContrast = True
    generateActualWorldWithOppositeOutcome p q,
    generateCounterfactualWorld p q
    ]

generateActualWorldWithPresupps :: TensedProp -> WorldTime
generateActualWorldWithPresupps p = (World (presuppositions p) False, Present)

generateActualWorldWithConditional :: TensedProp -> TensedProp -> WorldTime
generateActualWorldWithConditional p q = (World ([prop p, prop q] ++ presuppositions p ++ presuppositions q) False, time p)

generateActualWorldWithOppositeOutcome :: TensedProp -> TensedProp -> WorldTime
generateActualWorldWithOppositeOutcome p q = (World ([prop p, (negateProp . prop) q] ++ presuppositions p ++ presuppositions q) False, PastOrPresent)

generateActualWorldForCounterfactual :: TensedProp -> TensedProp -> WorldTime
generateActualWorldForCounterfactual p q = (World ([(negateProp . prop) p, (negateProp . prop) q] ++ presuppositions p ++ presuppositions q) False, time p)

generateCounterfactualWorld :: TensedProp -> TensedProp -> WorldTime
generateCounterfactualWorld p q = (World ([prop p, prop q] ++ presuppositions p ++ presuppositions q) True, time p)


-- TODO n.B. model generation above can't handle mixed time counterfactuals where the antecedent and consequent happen at different times
-- e.g. "If Charlie pays attention in class today, he'll pass the test tomorrow"
--  (due to data structures being too rigid - primarily binary switch of possibility rather than allowing one possible world to have a timeline)
validateTime :: Conditional -> Bool
validateTime (Conditional p q mood timeContrast) = (time p) == (time q)