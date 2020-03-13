module Examples where

import DataStructures

charlieTestYesterday = TensedProp {
    prop = Prop {
        content = "Charlie takes his test"
        , negated = False
    }
    , timeDelta = -1
}

charlieTestTomorrow = TensedProp {
    prop = Prop {
        content = "Charlie takes his test"
        , negated = False
    }
    , timeDelta = 1
}

charliePassTomorrow = TensedProp {
    prop = Prop {
        content = "Charlie passes his test"
        , negated = False
    }
    , timeDelta = 1
}

charliePassYesterday = TensedProp {
    prop = Prop {
        content = "Charlie passes his test"
        , negated = False
    }
    , timeDelta = -1
}

charlieNotFailTomorrow = TensedProp {
    prop = Prop {
        content = "Charlie fails his test"
        , negated = False
    }
    , timeDelta = 1
}

indicativeConditional = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Indicative
}

subjunctiveConditional = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Subjunctive
}

pastCounterfactual = Conditional {
    antecedent = charlieTestYesterday
    , consequent = charliePassYesterday
    , mood = Counterfactual
}

futureCounterfactual = Conditional {
    antecedent = charlieTestTomorrow
    , consequent = charliePassTomorrow
    , mood = Counterfactual
}