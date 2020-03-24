module Grammar.Features where

-- Let's not deal with the verb 'be' here which distinguishes extra forms
data VerbForm = PresentTense | Present3Sg | PastTense | EnEd | Inf | Modal deriving (Show, Eq)
data SgPl = Sg | Pl deriving (Show, Eq)
data PredicateProps = Repetition deriving (Show, Eq, Ord) -- special properties of predicate, for now, only one is relevant;
                                                          -- Ord required for ordering Props
data AuxEffect = FutureEffect | Perfect | Possible | NoEffect deriving (Show, Eq)
data Mood = Indicative | Subjunctive | Counterfactual deriving (Eq, Show) -- Abbreviated list of moods suitable for distinguishing conditionals
