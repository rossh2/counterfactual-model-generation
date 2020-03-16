module ModelGeneration
    ( generateModel
    , validateTime
    ) where

import DataStructures

-- TODO double check conditions for presuppositions of indicative vs. types of subjunctive against Ippolito, Starr
--   What is the difference between indicative conditionals and subjunctive (non-counterfactual) conditionals? (Provided we are not dealing with repeated events)

generateModel :: Conditional -> MinimalModel
generateModel (Conditional p q Indicative False) = [ -- timeFocused = False
    generateActualWorldWithPresupps p,
    generateActualWorldWithConditional p q
    ]
generateModel (Conditional p q Indicative True) = [ -- timeFocused = True
    generateActualWorldWithOppositeOutcome p q,
    generateActualWorldWithPresupps p,
    generateActualWorldWithConditional p q
    ]
generateModel (Conditional p q Subjunctive False) = [ -- timeFocused = False
    generateActualWorldWithPresupps p,
    generateActualWorldWithConditional p q
    ]
generateModel (Conditional p q Subjunctive True) = [ -- timeFocused = True
    generateActualWorldWithOppositeOutcome p q,
    generateActualWorldWithPresupps p,
    generateActualWorldWithConditional p q
    ]
generateModel (Conditional p q Counterfactual False) = [ -- timeFocused = False
    generateActualWorldForCounterfactual p q,
    generateCounterfactualWorld p q
    ]
generateModel (Conditional p q Counterfactual True) = [ -- timeFocused = True
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


-- TODO model generation above can't handle mixed time counterfactuals where the antecedent and consequent happen at different times
--  (due to data structures being too rigid - primarily binary switch of possibility rather than allowing one possible world to have a timeline)
validateTime :: Conditional -> Bool
validateTime (Conditional p q mood timeFocused) = (time p) == (time q)