module Examples where

import DataStructures

----------------------------------
-- PROPOSITIONS (WITH METADATA) --
----------------------------------

charlieCalculatorTomorrow = TensedProp {
    prop = Prop {
        content = "Charlie brings his calculator"
        , negated = False
    }
    , time = DeltaInDays 1
    , presuppositions = [
            Prop {
                content = "Charlie owns a calculator"
                , negated = False
            }
        ]
}

charlieCalculatorYesterday = TensedProp {
    prop = Prop {
        content = "Charlie brings his calculator"
        , negated = False
    }
    , time = DeltaInDays (-1)
    , presuppositions = [
            Prop {
                content = "Charlie owns a calculator"
                , negated = False
            }
        ]
}

charlieTestYesterday = TensedProp {
    prop = Prop {
        content = "Charlie takes his test"
        , negated = False
    }
    , time = DeltaInDays (-1)
    , presuppositions = [
        Prop {
            content = "Charlie has taken his test" -- n.B. treat perfective aspect as not a tense / not related to time
            , negated = True
        }
    ]
}

charlieTestTomorrow = TensedProp {
    prop = Prop {
        content = "Charlie takes his test"
        , negated = False
    }
    , time = DeltaInDays 1
    , presuppositions = [
        Prop {
            content = "Charlie has taken his test" -- n.B. treat perfective aspect as not a tense / not related to time
            , negated = True
        }
    ]
}

charlieRetakeTestTomorrow = TensedProp {
    prop = Prop {
        content = "Charlie re-takes his test"
        , negated = False
    }
    , time = DeltaInDays 1
    , presuppositions = [
        Prop {
            content = "Charlie has taken his test" -- n.B. treat perfective aspect as not a tense / not related to time
            , negated = False
        }
    ]
}

charliePassTomorrow = TensedProp {
    prop = Prop {
        content = "Charlie passes his test"
        , negated = False
    }
    , time = DeltaInDays 1
    , presuppositions = [] -- TODO this isn't strictly true, but in fact already covered by presupp's of antecedent (same for all consequents below)
}

charliePassYesterday = TensedProp {
    prop = Prop {
        content = "Charlie passes his test"
        , negated = False
    }
    , time = DeltaInDays (-1)
    , presuppositions = []
}

charliePassPast = TensedProp {
    prop = Prop {
        content = "Charlie passes his test"
        , negated = False
    }
    , time = PastOrPresent -- TODO required to make it clash with model generation from conditional/counterfactual
                           -- which doesn't understand that any time in past or present would clash with this (same below)
    , presuppositions = []
}

charlieFailPast = TensedProp {
      prop = Prop {
          content = "Charlie fails his test"
          , negated = False
      }
      , time = PastOrPresent
      , presuppositions = []
}

charlieNotFailTomorrow = TensedProp {
    prop = Prop {
        content = "Charlie fails his test"
        , negated = True
    }
    , time = DeltaInDays 1
    , presuppositions = []
}

charlieNotFailYesterday = TensedProp {
    prop = Prop {
        content = "Charlie fails his test"
        , negated = True
    }
    , time = DeltaInDays (-1)
    , presuppositions = []
}

charlieNotFailPast = TensedProp {
    prop = Prop {
        content = "Charlie fails his test"
        , negated = True
    }
    , time = PastOrPresent
    , presuppositions = []
}

--------------------------------
-- SENTENCES AND CONDITIONALS --
--------------------------------

passPastSentence = SimpleSentence {
    tprop = charliePassPast
}

failPastSentence = SimpleSentence {
    tprop = charlieFailPast
}

notFailPastSentence = SimpleSentence {
    tprop = charlieNotFailPast
}

-- "If Charlie brings his calculator tomorrow, Charlie will pass his test."
indicativePassConditional = Conditional {
    antecedent = charlieCalculatorTomorrow
    , consequent = charliePassTomorrow
    , mood = Indicative
    , timeContrast = False
}

-- "If Charlie brings his calculator tomorrow, Charlie won't fail his test."
indicativeNotFailConditional = Conditional {
    antecedent = charlieCalculatorTomorrow
    , consequent = charlieNotFailTomorrow
    , mood = Indicative
    , timeContrast = False
}

-- "If Charlie takes his test tomorrow, Charlie will pass his test."
indicativeTimeNoFocusPassConditional = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Indicative
    , timeContrast = False
}

-- "If Charlie takes his test tomorrow, Charlie won't fail his test."
indicativeTimeNoFocusNotFailConditional = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charlieNotFailTomorrow
    , mood = Indicative
    , timeContrast = False
}

-- "If Charlie re-takes his test *tomorrow*, Charlie will pass his test."
indicativeTimeFocusPassConditional = Conditional {
    antecedent = charlieRetakeTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Indicative
    , timeContrast = True
}

-- "If Charlie re-takes his test *tomorrow*, Charlie won't fail his test."
indicativeTimeFocusNotFailConditional = Conditional {
    antecedent = charlieRetakeTestTomorrow
    , consequent = charlieNotFailTomorrow
    , mood = Indicative
    , timeContrast = True
}

-- "If Charlie brought his calculator tomorrow, Charlie would pass his test."
subjunctivePassConditional = Conditional {
    antecedent = charlieCalculatorTomorrow
    , consequent = charliePassTomorrow
    , mood = Subjunctive
    , timeContrast = False
}

-- "If Charlie brought his calculator tomorrow, Charlie wouldn't fail his test."
subjunctiveNotFailConditional = Conditional {
    antecedent = charlieCalculatorTomorrow
    , consequent = charlieNotFailTomorrow
    , mood = Subjunctive
    , timeContrast = False
}

-- "If Charlie re-took his test *tomorrow*, Charlie would pass his test."
subjunctiveTimeFocusPassConditional = Conditional {
    antecedent = charlieRetakeTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Subjunctive
    , timeContrast = True
}

-- "If Charlie re-took his test *tomorrow*, Charlie wouldn't fail his test."
subjunctiveTimeFocusNotFailConditional = Conditional {
    antecedent = charlieRetakeTestTomorrow
    , consequent = charlieNotFailTomorrow
    , mood = Subjunctive
    , timeContrast = True
}

-- "If Charlie had brought his calculator yesterday, Charlie would have passed his test."
pastPassCounterfactual = Conditional {
    antecedent = charlieCalculatorYesterday
    , consequent = charliePassYesterday
    , mood = Counterfactual
    , timeContrast = False
}

-- "If Charlie had brought his calculator yesterday, Charlie wouldn't have failed his test."
pastNotFailCounterfactual = Conditional {
    antecedent = charlieCalculatorYesterday
    , consequent = charlieNotFailYesterday
    , mood = Counterfactual
    , timeContrast = False
}

-- "If Charlie had taken his test *yesterday*, Charlie would have passed his test."
pastTimeFocusPassCounterfactual = Conditional {
    antecedent = charlieTestYesterday
    , consequent = charliePassYesterday
    , mood = Counterfactual
    , timeContrast = True
}

-- "If Charlie had taken his test *yesterday*, Charlie wouldn't have failed his test."
pastTimeFocusNotFailCounterfactual = Conditional {
    antecedent = charlieTestYesterday
    , consequent = charlieNotFailYesterday
    , mood = Counterfactual
    , timeContrast = True
}

-- "If Charlie had taken his test *tomorrow*, Charlie would have passed his test."
futureTimeFocusPassCounterfactual = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Counterfactual
    , timeContrast = True
}

-- "If Charlie had taken his test *tomorrow*, Charlie wouldn't have failed his test."
futureTimeFocusNotFailCounterfactual = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charlieNotFailTomorrow
    , mood = Counterfactual
    , timeContrast = True
}