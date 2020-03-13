module Examples where

import DataStructures

charlieCalculatorTomorrow = TensedProp {
    prop = Prop {
        content = "Charlie brings his calculator"
        , negated = False
    }
    , time = Tomorrow
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
    , time = Yesterday
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
    , time = Yesterday
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
    , time = Tomorrow
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
    , time = Tomorrow
    , presuppositions = [] -- TODO this isn't true, but in fact already covered by presupp's of antecedent
}

charliePassYesterday = TensedProp {
    prop = Prop {
        content = "Charlie passes his test"
        , negated = False
    }
    , time = Yesterday
    , presuppositions = [] -- TODO this isn't true, but in fact already covered by presupp's of antecedent
}

charlieNotFailTomorrow = TensedProp {
    prop = Prop {
        content = "Charlie fails his test"
        , negated = False
    }
    , time = Tomorrow
    , presuppositions = [] -- TODO this isn't true, but in fact already covered by presupp's of antecedent
}

-- "If Charlie brings his calculator tomorrow, Charlie will pass his test."
indicativeConditional = Conditional {
    antecedent = charlieCalculatorTomorrow
    , consequent = charliePassTomorrow
    , mood = Indicative
    , timeFocused = False
}

-- "If Charlie takes his test tomorrow, Charlie will pass his test."
indicativeTimeNoFocusConditional = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Indicative
    , timeFocused = False
}

-- "If Charlie re-takes his test *tomorrow*, Charlie will pass his test."
indicativeTimeFocusConditional = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Indicative
    , timeFocused = True
}

-- "If Charlie brought his calculator tomorrow, Charlie would pass his test."
subjunctiveConditional = Conditional {
    antecedent = charlieCalculatorTomorrow
    , consequent = charliePassTomorrow
    , mood = Subjunctive
    , timeFocused = False
}

-- "If Charlie re-took his test *tomorrow*, Charlie would pass his test."
subjunctiveTimeFocusConditional = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Subjunctive
    , timeFocused = True
}

-- "If Charlie had brought his calculator yesterday, Charlie would have passed his test."
pastCounterfactual = Conditional {
    antecedent = charlieCalculatorYesterday
    , consequent = charliePassYesterday
    , mood = Counterfactual
    , timeFocused = False
}

-- "If Charlie had taken his test *yesterday*, Charlie would have passed his test."
pastTimeFocusCounterfactual = Conditional {
    antecedent = charlieTestYesterday
    , consequent = charliePassYesterday
    , mood = Counterfactual
    , timeFocused = True
}

-- "If Charlie had taken his test *tomorrow*, Charlie would have passed his test."
futureTimeFocusCounterfactual = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Counterfactual
    , timeFocused = True
}