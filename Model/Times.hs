module Model.Times where

import Grammar.Features
import Grammar.Lexicon
import Utils.TypeClasses

data Time = Past | PastOrPresent | Present | Future | DeltaInDays Int -- Delta from Present, e.g. -1 for yesterday, +1 for tomorrow
    -- in LTL: Past = \exists t < t_0, PastOrPresent = \exists t <= t_0, Present = t_0, Future = \exists t > t_0
    deriving (Show, Eq) -- Ord declared below

instance ShowLinear Time where
    showLin Past = "Past"
    showLin PastOrPresent = "Past or Present"
    showLin Present = "Present"
    showLin Future = "Future"
    showLin (DeltaInDays 1) = "Tomorrow"
    showLin (DeltaInDays (-1)) = "Yesterday"
    showLin (DeltaInDays n) = if n > 0 then (dayOrDays n) ++ " from today" else (dayOrDays (-n)) ++ " ago"

dayOrDays :: Int -> String
dayOrDays n = if n == 1 then "1 day" else (show n) ++ " days"

instance Ord Time where
    (<=) Past            t               = True
    -- It's not clear where PastOrPresent should sit, but for the purpose of this ordering imagine that it sits
    -- a tiny delta of time before the present. So any definitely past time is earlier than it,
    -- but the (definitely) present is later than it, and no definite time is equal to it.
    (<=) PastOrPresent   Past            = False
    (<=) PastOrPresent   PastOrPresent   = True
    (<=) PastOrPresent   Present         = True
    (<=) PastOrPresent   Future          = True
    (<=) PastOrPresent   (DeltaInDays n) = n >= 0
    (<=) Present         Past            = False
    (<=) Present         PastOrPresent   = False
    (<=) Present         Present         = True
    (<=) Present         Future          = True
    (<=) Present         (DeltaInDays n) = n >= 0
    (<=) Future          Future          = True
    (<=) Future          t               = False
    (<=) (DeltaInDays n) Past            = False
    (<=) (DeltaInDays n) PastOrPresent   = n < 0
    (<=) (DeltaInDays n) Present         = n <= 0
    (<=) (DeltaInDays n) Future          = True


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
verbFormsToTime [Possible] [Modal, Inf] = Just Future -- unclear how to map possibility to time, but "would do" only compatible with future adverbs
-- TODO "would have done" appears to be compatible with all times so this represents an unknown value
verbFormsToTime [Possible, Perfect] [Modal, Inf, EnEd] = Just PastOrPresent
verbFormsToTime [NoEffect] xs = verbFormsToTime [] xs
verbFormsToTime _ _ = Nothing -- Ungrammatical combination

-- Basic mapping of Aux meanings to the moods used to identify conditionals
auxEffectsToMood :: [AuxEffect] -> Maybe Mood
auxEffectsToMood [Possible] = Just Subjunctive -- "would go"
auxEffectsToMood [Possible, Perfect] = Just Counterfactual -- "would have gone"
-- treat "if I had gone" as indicative on its own, since the conditional could be "if James had gone (already), then Ilana had gone (already)"
auxEffectsToMood _ = Just Indicative

advNToTime :: AdverbialN -> Maybe Time
advNToTime advN = case advNHead advN of "tomorrow" -> Just (DeltaInDays 1)
                                        "yesterday" -> Just (DeltaInDays (-1))
                                        _ -> Nothing
