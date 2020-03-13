module Main where

import Data
import Lib

charlieTestYesterday = Prop {
    content = "Charlie takes his test yesterday"
    , negated = False
}

charlieTestTomorrow = Prop {
    content = "Charlie takes his test tomorrow"
    , negated = False
}

charliePass = Prop {
    content = "Charlie passes his test"
    , negated = False
}

charlieNotFail = Prop {
    content = "Charlie fails his test"
    , negated = True
}

indicativeConditional = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePass
    , mood = Indicative
}

subjunctiveConditional = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePass
    , mood = Subjunctive
}

pastCounterfactual = Conditional {
    antecedent = charlieTestYesterday
    , consequent = charliePass
    , mood = Counterfactual
}

futureCounterfactual = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePass
    , mood = Counterfactual
}

main :: IO ()
main =  (putStr . show . generateModel) pastCounterfactual
