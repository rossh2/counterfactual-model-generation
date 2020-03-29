module Test.PropExamples where

import Grammar.Features
import Model.Times
import Parsing.DataStructures
import Test.TreeExamples

------------------
-- PROPOSITIONS --
------------------

charlieOwnCalculatorPast = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "own"
    , arguments = ["a calculator"]
    , time = Past
    , predProps = []
    })


charlieOwnCalculatorTomorrow = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "own"
    , arguments = ["a calculator"]
    , time = DeltaInDays 1
    , predProps = []
    })

charlieBringCalculatorPast = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "bring"
    , arguments = ["his calculator"]
    , time = Past
    , predProps = []
    })

charlieBringCalculatorTomorrow = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "bring"
    , arguments = ["his calculator"]
    , time = DeltaInDays 1
    , predProps = []
    })

charlieTakeTestPast = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "take"
    , arguments = ["the test"]
    , time = Past
    , predProps = []
    })

charlieTakeTestYesterday = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "take"
    , arguments = ["the test"]
    , time = DeltaInDays (-1)
    , predProps = []
    })

charlieTakeTestTomorrow = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "take"
    , arguments = ["the test"]
    , time = DeltaInDays 1
    , predProps = []
    })

charlieNotTakeTestPast = Prop {
    event = Event {
        agent = "Charlie"
        , predicate = "take"
        , arguments = ["the test"]
        , time = Past
        , predProps = []
    }
    , negated = True
    , cancellable = False
    }

charlieRetakeTestTomorrow = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "retake" -- TODO is this the same or not as "Charlie take(s) the test"? In model generation, should we get [take, retake] as separate propositions
    , arguments = ["the test"]
    , time = DeltaInDays 1
    , predProps = [Repetition]
    })

charliePassTestPast = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "pass"
    , arguments = ["the test"]
    , time = Past
    , predProps = []
    })

charliePassTestYesterday = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "pass"
    , arguments = ["the test"]
    , time = DeltaInDays (-1)
    , predProps = []
    })

charliePassTestTomorrow = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "pass"
    , arguments = ["the test"]
    , time = DeltaInDays 1
    , predProps = []
    })

charlieNotPassTestPast = Prop {
    event = Event {
        agent = "Charlie"
        , predicate = "pass"
        , arguments = ["the test"]
        , time = Past
        , predProps = []
    }
    , negated = True
    , cancellable = False
    }

charlieNotPassTestYesterday = Prop {
    event = Event {
        agent = "Charlie"
        , predicate = "pass"
        , arguments = ["the test"]
        , time = DeltaInDays (-1)
        , predProps = []
    }
    , negated = True
    , cancellable = False
    }

charlieNotPassTestTomorrow = Prop {
    event = Event {
        agent = "Charlie"
        , predicate = "pass"
        , arguments = ["the test"]
        , time = DeltaInDays 1
        , predProps = []
    }
    , negated = True
    , cancellable = False
    }

charlieFailTestPast = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "fail"
    , arguments = ["the test"]
    , time = Past
    , predProps = []
    })

charlieFailTestYesterday = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "fail"
    , arguments = ["the test"]
    , time = DeltaInDays (-1)
    , predProps = []
    })

charlieFailTestTomorrow = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "fail"
    , arguments = ["the test"]
    , time = DeltaInDays 1
    , predProps = []
    })

charlieNotFailTestPast = Prop {
    event = Event {
        agent = "Charlie"
        , predicate = "fail"
        , arguments = ["the test"]
        , time = Past
        , predProps = []
    }
    , negated = True
    , cancellable = False
    }

charlieNotFailTestYesterday = Prop {
    event = Event {
        agent = "Charlie"
        , predicate = "fail"
        , arguments = ["the test"]
        , time = DeltaInDays (-1)
        , predProps = []
    }
    , negated = True
    , cancellable = False
    }

charlieNotFailTestTomorrow = Prop {
    event = Event {
        agent = "Charlie"
        , predicate = "fail"
        , arguments = ["the test"]
        , time = DeltaInDays 1
        , predProps = []
    }
    , negated = True
    , cancellable = False
    }

-------------------------
-- PARSED PROPOSITIONS --
-------------------------

charliePassPast = ParsedProp {
    prop = charliePassTestPast
    , mood = Indicative
    , presuppositions = [ charlieTakeTestPast ]
    , tree = charliePassedTheTest
    }

charlieNotPassPast = ParsedProp {
    prop = charlieNotPassTestPast
    , mood = Indicative
    , presuppositions = [ charlieTakeTestPast ]
    , tree = charlieDidNotPassTheTest
    }

charlieFailPast = ParsedProp {
    prop = charlieFailTestPast
    , mood = Indicative
    , presuppositions = [ charlieTakeTestPast ]
    , tree = charlieFailedTheTest
    }

charlieNotFailPast = ParsedProp {
    prop = charlieNotFailTestPast
    , mood = Indicative
    , presuppositions = [ charlieTakeTestPast ]
    , tree = charlieDidNotFailTheTest
    }

-- "Charlie brings his calculator tomorrow"
charlieBringCalculatorTomorrowIndicative = ParsedProp {
    prop = charlieBringCalculatorTomorrow
    , mood = Indicative
    , presuppositions = [ charlieOwnCalculatorTomorrow ]
    , tree = charlieBringsHisCalculatorTomorrow
    }

-- "Charlie takes the test tomorrow"
charlieTakeTestTomorrowIndicative = ParsedProp {
    prop = charlieTakeTestTomorrow
    , mood = Indicative
    , presuppositions = [ charlieNotTakeTestPast ]
    , tree = charlieTakesTheTestTomorrow
    }

-- "Charlie re-takes the test tomorrow"
charlieRetakeTestTomorrowIndicative = ParsedProp {
    prop = charlieRetakeTestTomorrow
    , mood = Indicative
    , presuppositions = [ charlieTakeTestPast ]
    , tree = charlieRetakesTheTestTomorrow
    }

-- "Charlie will pass the test"
charliePassTomorrowIndicative = ParsedProp {
    prop = charliePassTestTomorrow
    , mood = Indicative
    , presuppositions = [ charlieTakeTestTomorrow ]
    , tree = charlieWillPassTheTest
    }

-- "Charlie will not pass the test"
charlieNotPassTomorrowIndicative = ParsedProp {
    prop = charlieNotPassTestTomorrow
    , mood = Indicative
    , presuppositions = [ charlieTakeTestTomorrow ]
    , tree = charlieWillNotPassTheTest
    }

-- "Charlie will fail the test"
charlieFailTomorrowIndicative = ParsedProp {
    prop = charlieFailTestTomorrow
    , mood = Indicative
    , presuppositions = [ charlieTakeTestTomorrow ]
    , tree = charlieWillFailTheTest
    }

-- "Charlie will not fail the test"
charlieNotFailTomorrowIndicative = ParsedProp {
    prop = charlieNotFailTestTomorrow
    , mood = Indicative
    , presuppositions = [ charlieTakeTestTomorrow ]
    , tree = charlieWillNotFailTheTest
    }

-- "[If] Charlie brought his calculator tomorrow"
charlieBringCalculatorTomorrowSubjunctive = ParsedProp {
    prop = charlieBringCalculatorTomorrow
    , mood = Indicative -- Technically the mood is indicative (or at least we can't tell) but this is the antecedent for subjunctive conditionals
    , presuppositions = [ charlieOwnCalculatorTomorrow ]
    , tree = charlieBroughtHisCalculatorTomorrow
    }

-- "[If] Charlie took the test tomorrow"
charlieTakeTestTomorrowSubjunctive = ParsedProp {
    prop = charlieTakeTestTomorrow
    , mood = Indicative -- Technically (see above)
    , presuppositions = [ charlieNotTakeTestPast ]
    , tree = charlieTookTheTestTomorrow
    }

-- "[If] Charlie re-took the test tomorrow"
charlieRetakeTestTomorrowSubjunctive = ParsedProp {
    prop = charlieRetakeTestTomorrow
    , mood = Indicative -- Technically (see above)
    , presuppositions = [ charlieTakeTestPast ]
    , tree = charlieRetookTheTestTomorrow
    }

-- "Charlie would pass the test"
charliePassTomorrowSubjunctive = ParsedProp {
    prop = charliePassTestTomorrow
    , mood = Subjunctive
    , presuppositions = [ charlieTakeTestTomorrow ]
    , tree = charlieWouldPassTheTest
    }

-- "Charlie would not pass the test"
charlieNotPassTomorrowSubjunctive = ParsedProp {
    prop = charlieNotPassTestTomorrow
    , mood = Subjunctive
    , presuppositions = [ charlieTakeTestTomorrow ]
    , tree = charlieWouldNotPassTheTest
    }

-- "Charlie would fail the test"
charlieFailTomorrowSubjunctive = ParsedProp {
    prop = charlieFailTestTomorrow
    , mood = Subjunctive
    , presuppositions = [ charlieTakeTestTomorrow ]
    , tree = charlieWouldFailTheTest
    }

-- "Charlie would not fail the test"
charlieNotFailTomorrowSubjunctive = ParsedProp {
    prop = charlieNotFailTestTomorrow
    , mood = Subjunctive
    , presuppositions = [ charlieTakeTestTomorrow ]
    , tree = charlieWouldNotFailTheTest
    }

-- "[If] Charlie had brought his calculator [when he took the test]"
charlieBringCalculatorPastCounterfactual = ParsedProp {
    prop = charlieBringCalculatorPast
    , mood = Indicative -- Technically the mood is indicative (or at least we can't tell) but this is the antecedent for subjunctive conditionals
    , presuppositions = [ charlieOwnCalculatorPast ]
    , tree = charlieHadBroughtHisCalculator
    }

-- "[If] Charlie had taken the test yesterday"
charlieTakeTestYesterdayCounterfactual = ParsedProp {
    prop = charlieTakeTestYesterday
    , mood = Indicative -- Technically (see above)
    , presuppositions = [ charlieNotTakeTestPast ]
    , tree = charlieHadTakenTheTestYesterday
    }

-- "[If] Charlie had taken the test tomorrow"
charlieTakeTestTomorrowCounterfactual = ParsedProp {
    prop = charlieTakeTestTomorrow
    , mood = Indicative -- Technically (see above)
    , presuppositions = [ charlieNotTakeTestPast ]
    , tree = charlieHadTakenTheTestTomorrow
    }

-- "Charlie would have passed the test"
charliePassPastCounterfactual = ParsedProp {
    prop = charliePassTestPast
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTestPast ]
    , tree = charlieWouldHavePassedTheTest
    }

-- "Charlie would have passed the test"
charliePassYesterdayCounterfactual = ParsedProp {
    prop = charliePassTestYesterday
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTestYesterday ]
    , tree = charlieWouldHavePassedTheTest
    }

-- "Charlie would have passed the test"
charliePassTomorrowCounterfactual = ParsedProp {
    prop = charliePassTestTomorrow
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTestTomorrow ]
    , tree = charlieWouldHavePassedTheTest
    }

-- "Charlie would not have passed the test"
charlieNotPassPastCounterfactual = ParsedProp {
    prop = charlieNotPassTestPast
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTestPast ]
    , tree = charlieWouldNotHavePassedTheTest
    }

-- "Charlie would not have passed the test"
charlieNotPassYesterdayCounterfactual = ParsedProp {
    prop = charlieNotPassTestYesterday
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTestYesterday ]
    , tree = charlieWouldNotHavePassedTheTest
    }

-- "Charlie would not have passed the test"
charlieNotPassTomorrowCounterfactual = ParsedProp {
    prop = charlieNotPassTestTomorrow
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTestTomorrow ]
    , tree = charlieWouldNotHavePassedTheTest
    }

-- "Charlie would have failed the test"
charlieFailPastCounterfactual = ParsedProp {
    prop = charlieFailTestPast
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTestPast ]
    , tree = charlieWouldHaveFailedTheTest
    }

-- "Charlie would have failed the test"
charlieFailYesterdayCounterfactual = ParsedProp {
    prop = charlieFailTestYesterday
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTestYesterday ]
    , tree = charlieWouldHaveFailedTheTest
    }

-- "Charlie would have failed the test"
charlieFailTomorrowCounterfactual = ParsedProp {
    prop = charlieFailTestTomorrow
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTestTomorrow ]
    , tree = charlieWouldHaveFailedTheTest
    }

-- "Charlie wouldn't have failed the test"
charlieNotFailPastCounterfactual = ParsedProp {
    prop = charlieNotFailTestPast
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTestPast ]
    , tree = charlieWouldNotHaveFailedTheTest
    }

-- "Charlie wouldn't have failed the test"
charlieNotFailYesterdayCounterfactual = ParsedProp {
    prop = charlieNotFailTestYesterday
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTestYesterday ]
    , tree = charlieWouldNotHaveFailedTheTest
    }

-- "Charlie wouldn't have failed the test"
charlieNotFailTomorrowCounterfactual = ParsedProp {
    prop = charlieNotFailTestTomorrow
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTestTomorrow ]
    , tree = charlieWouldNotHaveFailedTheTest
    }