module Examples where

import DataStructures

charlieCalculatorTomorrow = TensedProp {
    prop = Prop {
        content = "Charlie brings his calculator"
        , negated = False
    }
    , timeDelta = 1 -- tomorrow
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
    , timeDelta = -1 -- yesterday
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
    , timeDelta = -1 -- yesterday
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
    , timeDelta = 1 -- tomorrow
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
    , timeDelta = 1
    , presuppositions = [] -- TODO this isn't true, but in fact already covered by presupp's of antecedent
}

charliePassYesterday = TensedProp {
    prop = Prop {
        content = "Charlie passes his test"
        , negated = False
    }
    , timeDelta = -1
    , presuppositions = [] -- TODO this isn't true, but in fact already covered by presupp's of antecedent
}

charlieNotFailTomorrow = TensedProp {
    prop = Prop {
        content = "Charlie fails his test"
        , negated = False
    }
    , timeDelta = 1
    , presuppositions = [] -- TODO this isn't true, but in fact already covered by presupp's of antecedent
}

-- "If Charlie brings his calculator tomorrow, Charlie will pass his test."
indicativeConditional = Conditional {
    antecedent = charlieCalculatorTomorrow
    , consequent = charliePassTomorrow
    , mood = Indicative
}

-- "If Charlie takes his test *tomorrow*, Charlie will pass his test."
indicativeTimeFocusConditional = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Indicative
}

-- "If Charlie brought his calculator tomorrow, Charlie would pass his test."
subjunctiveConditional = Conditional {
    antecedent = charlieCalculatorTomorrow
    , consequent = charliePassTomorrow
    , mood = Subjunctive
}

-- "If Charlie took his test *tomorrow*, Charlie would pass his test."
subjunctiveTimeFocusConditional = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Subjunctive
}

-- "If Charlie had brought his calculator yesterday, Charlie would have passed his test."
pastCounterfactual = Conditional {
    antecedent = charlieCalculatorYesterday
    , consequent = charliePassYesterday
    , mood = Counterfactual
}

-- "If Charlie had taken his test *yesterday*, Charlie would have passed his test."
pastTimeFocusCounterfactual = Conditional {
    antecedent = charlieTestYesterday
    , consequent = charliePassYesterday
    , mood = Counterfactual
}

-- "If Charlie had taken his test *tomorrow*, Charlie would have passed his test."
futureTimeFocusCounterfactual = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Counterfactual
}