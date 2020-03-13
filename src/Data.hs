module Data
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Prop = Prop {
    content :: String
    , negated :: Bool
} deriving (Show)

data Mood = Indicative | Subjunctive | Counterfactual deriving (Eq, Show, Enum)

data Conditional = Conditional {
    antecedent :: Prop
    , consequent :: Prop
    , mood :: Mood
}
