module ModelGeneration
    ( generateModel
    ) where

import DataStructures

-- TODO double check conditions for presuppositions of indicative vs. types of subjunctive against Ippolito, Starr
--   What is the difference between indicative conditionals and subjunctive (non-counterfactual) conditionals? (Provided we are not dealing with repeated events)

generateModel :: Conditional -> MinimalModel
generateModel (Conditional p q Indicative) = [
    (World (presuppositions p) False, now),
    (World ([prop p, prop q] ++ presuppositions p ++ presuppositions q) False, getTime p now)
    ]
generateModel (Conditional p q Subjunctive) = [
    (World (presuppositions p) False, now),
    (World ([prop p, prop q] ++ presuppositions p ++ presuppositions q) False, getTime p now)
    ]
generateModel (Conditional p q Counterfactual) = [
    -- TODO this can't handle mixed time counterfactuals where the antecedent and consequent happen at different times
    --  (due to data structures being too rigid - primarily binary switch of possibility rather than allowing one possible world to have a timeline)
    (World ([(negateProp . prop) p, (negateProp . prop) q] ++ presuppositions p ++ presuppositions q) False, getTime p now),
    (World ([prop p, prop q] ++ presuppositions p ++ presuppositions q) True, getTime p now)
    ]
    -- !!! This doesn't work: it generates the fact that Charlie does not take his test at time now and that he doesn't pass
    -- whereas what we want is that Charlie didn't take his test at the time specified, but did take it at some other time
    -- TODO add time focus component which causes generateModel to generate Charlie taking the test at some other (past) unknown time - will need more complex time handling for that first

getTime :: TensedProp -> Time -> Time
getTime p t = (timeDelta p) + t
