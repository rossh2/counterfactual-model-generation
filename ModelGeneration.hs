module ModelGeneration
    ( generateModel
    , validateTime
    ) where

import DataStructures

-- TODO double check conditions for presuppositions of indicative vs. types of subjunctive against Ippolito, Starr
--   What is the difference between indicative conditionals and subjunctive (non-counterfactual) conditionals? (Provided we are not dealing with repeated events)

generateModel :: Conditional -> MinimalModel
generateModel (Conditional p q Indicative False) = [ -- timeFocused = False
    (World (presuppositions p) False, Present),
    (World ([prop p, prop q] ++ presuppositions p ++ presuppositions q) False, time p)
    ]
generateModel (Conditional p q Indicative True) = [ -- timeFocused = True
    (World ([prop p, (negateProp . prop) q] ++ presuppositions p ++ presuppositions q) False, PastOrPresent),
    (World (presuppositions p) False, Present),
    (World ([prop p, prop q] ++ presuppositions p ++ presuppositions q) False, time p)
    ]
generateModel (Conditional p q Subjunctive False) = [ -- timeFocused = False
    (World (presuppositions p) False, Present),
    (World ([prop p, prop q] ++ presuppositions p ++ presuppositions q) False, time p)
    ]
generateModel (Conditional p q Subjunctive True) = [ -- timeFocused = True
    (World ([prop p, (negateProp . prop) q] ++ presuppositions p ++ presuppositions q) False, PastOrPresent),
    (World (presuppositions p) False, Present),
    (World ([prop p, prop q] ++ presuppositions p ++ presuppositions q) False, time p)
    ]
generateModel (Conditional p q Counterfactual False) = [ -- timeFocused = False
    (World ([(negateProp . prop) p, (negateProp . prop) q] ++ presuppositions p ++ presuppositions q) False, time p),
    (World ([prop p, prop q] ++ presuppositions p ++ presuppositions q) True, time p)
    ]
generateModel (Conditional p q Counterfactual True) = [ -- timeFocused = True
    (World ([prop p, (negateProp . prop) q] ++ presuppositions p ++ presuppositions q) False, PastOrPresent),
    (World ([prop p, prop q] ++ presuppositions p ++ presuppositions q) True, time p)
    ]
    -- !!! This doesn't work: it generates the fact that Charlie does not take his test at time now and that he doesn't pass
    -- whereas what we want is that Charlie didn't take his test at the time specified, but did take it at some other time
    -- TODO add time focus component which causes generateModel to generate Charlie taking the test at some other (past) unknown time - will need more complex time handling for that first

-- TODO model generation above can't handle mixed time counterfactuals where the antecedent and consequent happen at different times
--  (due to data structures being too rigid - primarily binary switch of possibility rather than allowing one possible world to have a timeline)
validateTime :: Conditional -> Bool
validateTime (Conditional p q mood timeFocused) = (time p) == (time q)