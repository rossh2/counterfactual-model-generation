module Model.Times where

import Grammar.Features
import Grammar.Grammar
import Grammar.Lexicon
import Utils.TypeClasses

data Time = Past | Present | PresentOrFuture | Future | DeltaInDays Int | Unknown
    -- DeltaInDays: from Present, e.g. -1 for yesterday, +1 for tomorrow
    -- in LTL: Past = \exists t < t_0, Present = t_0, PresentOrFuture = \exists t >= t_0, Future = \exists t > t_0
    deriving (Show, Eq) -- Ord declared below

instance ShowLinear Time where
    showLin Past = "Past"
    showLin Present = "Present"
    showLin PresentOrFuture = "Present or Future"
    showLin Future = "Future"
    showLin (DeltaInDays 1) = "Tomorrow"
    showLin (DeltaInDays (-1)) = "Yesterday"
    showLin (DeltaInDays n) = if n > 0 then (dayOrDays n) ++ " from today" else (dayOrDays (-n)) ++ " ago"
    showLin Unknown = "Unknown"

dayOrDays :: Int -> String
dayOrDays n = if n == 1 then "1 day" else (show n) ++ " days"

instance Ord Time where
    (<=) Past            t               = True
    -- It's not clear where PresentOrFuture should sit, but for the purpose of this ordering imagine that it sits
    -- a tiny delta of time after the present. So any definitely future time is later than it,
    -- but the (definitely) present is earlier than it, and no definite time is equal to it.
    -- Unknown is impossible to put on a scale, but let's suppose it lies after Future.
    (<=) Present         Past            = False
    (<=) Present         Present         = True
    (<=) Present         PresentOrFuture = True
    (<=) Present         Future          = True
    (<=) Present         (DeltaInDays n) = n >= 0
    (<=) Present         Unknown         = True
    (<=) PresentOrFuture Past            = False
    (<=) PresentOrFuture Present         = False
    (<=) PresentOrFuture PresentOrFuture = True
    (<=) PresentOrFuture Future          = True
    (<=) PresentOrFuture Unknown         = True
    (<=) Future          Future          = True
    (<=) Future          Unknown         = True
    (<=) Future          t               = False
    (<=) (DeltaInDays n) Past            = False
    (<=) (DeltaInDays n) Present         = n <= 0
    (<=) (DeltaInDays n) PresentOrFuture = n <= 0
    (<=) (DeltaInDays n) Future          = True
    (<=) (DeltaInDays n) Unknown         = True
    (<=) Unknown         Unknown         = True
    (<=) Unknown         t               = False


isSpecificTime :: Time -> Bool
isSpecificTime Past = False
isSpecificTime Present = True
isSpecificTime PresentOrFuture = False
isSpecificTime Future = False
isSpecificTime (DeltaInDays n) = True
isSpecificTime Unknown = False


-- Somewhat ugly mapping of Aux meanings + tenses to time; effectively encodes subcat frames for auxs when destructuring
verbFormsToTime :: [AuxEffect] -> [VerbForm] -> Maybe Time
verbFormsToTime [] [PresentTense] = Just Present
verbFormsToTime [] [PresentTense, Inf] = Just Present -- for "do + Inf"
verbFormsToTime [] [Present3Sg] = Just Present
verbFormsToTime [] [Present3Sg, Inf] = Just Present -- for "does + Inf"
verbFormsToTime [] [PastTense] = Just Past
verbFormsToTime [] [PastTense, Inf] = Just Past -- for "did + Inf"
verbFormsToTime [FutureEffect] [Modal, Inf] = Just Future
verbFormsToTime [Perfect] [EnEd] = Just Past -- Perfective aspect has no effect on plain LTL logic
verbFormsToTime [Perfect] [PastTense, EnEd] = Just Past -- for "had brought"
-- unclear how to map possibility to time, but "would do" only compatible with future adverbs or present
verbFormsToTime [Possible] [Modal, Inf] = Just PresentOrFuture
-- "would have done" appears to be compatible with all times
verbFormsToTime [Possible, Perfect] [Modal, Inf, EnEd] = Just Unknown
verbFormsToTime [NoEffect] xs = verbFormsToTime [] xs
verbFormsToTime _ _ = Nothing -- Ungrammatical combination

-- Basic mapping of Aux meanings to the moods used to identify conditionals
auxEffectsToMood :: [AuxEffect] -> Maybe Mood
auxEffectsToMood [Possible] = Just Subjunctive -- "would go"
auxEffectsToMood [Possible, Perfect] = Just Counterfactual -- "would have gone"
-- treat "if I had gone" as indicative on its own, since the conditional could be "if James had gone (already), then Ilana had gone (already)"
auxEffectsToMood _ = Just Indicative

timeFromAdjunct :: VPAdjunct -> Maybe Time
-- If the lexicon contained PPs we'd also need to handle cases like "on Monday" here
timeFromAdjunct (VPA1 advN) = case advNHead advN of
                                "tomorrow" -> Just (DeltaInDays 1)
                                "yesterday" -> Just (DeltaInDays (-1))
                                _ -> Nothing
timeFromAdjunct (VPA2 adverb) = Nothing -- We don't have any adverbs in the lexicon that relate to time
