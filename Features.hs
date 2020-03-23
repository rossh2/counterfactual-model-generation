module Features where

import Times

-- Let's not deal with the verb 'be' here which distinguishes extra forms
data VerbForm = PresentTense | Present3Sg | PastTense | EnEd | Inf | Modal deriving (Show, Eq)
data SgPl = Sg | Pl deriving (Show, Eq)
data PredicateProps = Repetition deriving (Show, Eq, Ord) -- special properties of predicate, for now, only one is relevant; Ord required for ordering Props
data AuxEffect = FutureEffect | Perfect | Possible | NoEffect deriving (Show, Eq)
data Mood = Indicative | Subjunctive | Counterfactual deriving (Eq, Show) -- Abbreviated list of moods suitable for distinguishing conditionals

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
verbFormsToTime [Possible, Perfect] [Modal, Inf, EnEd] = Just PastOrPresent -- TODO "would have done" appears to be compatible with all times so this is a dummy value
verbFormsToTime [NoEffect] xs = verbFormsToTime [] xs
verbFormsToTime _ _ = Nothing -- Ungrammatical combination

-- Basic mapping of Aux meanings to the moods used to identify conditionals
auxEffectsToMood :: [AuxEffect] -> Maybe Mood
auxEffectsToMood [Possible] = Just Subjunctive -- "would go"
auxEffectsToMood [Possible, Perfect] = Just Counterfactual -- "would have gone"
auxEffectsToMood _ = Just Indicative -- treat "if I had gone" as indicative on its own, since the conditional could be "if James had gone (already), then Ilana had gone (already)"
