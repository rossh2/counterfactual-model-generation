module Examples where

import DataStructures

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

charlieNotFailTomorrow = TensedProp {
    prop = Prop {
        content = "Charlie fails his test"
        , negated = False
    }
    , time = DeltaInDays 1
    , presuppositions = []
}

charlieNotFailYesterday = TensedProp {
    prop = Prop {
        content = "Charlie fails his test"
        , negated = False
    }
    , time = DeltaInDays (-1)
    , presuppositions = []
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
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Indicative
    , timeContrast = True
}

-- "If Charlie re-takes his test *tomorrow*, Charlie won't fail his test."
indicativeTimeFocusNotFailConditional = Conditional {
    antecedent = charlieTestTomorrow
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
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Subjunctive
    , timeContrast = True
}

-- "If Charlie re-took his test *tomorrow*, Charlie wouldn't fail his test."
subjunctiveTimeFocusNotFailConditional = Conditional {
    antecedent = charlieTestTomorrow
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