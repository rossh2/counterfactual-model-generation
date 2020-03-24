module Test.PropExamples where

import Grammar.Features
import Model.Times
import Parsing.DataStructures
import Test.TreeExamples

------------------
-- PROPOSITIONS --
------------------

charlieOwnCalculator = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "own"
    , arguments = ["a calculator"]
    , predProps = []
    })

charlieBringCalculator = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "bring"
    , arguments = ["his calculator"]
    , predProps = []
    })

charlieTakeTest = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "take"
    , arguments = ["the test"]
    , predProps = []
    })

charlieRetakeTest = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "retake" -- TODO is this the same or not as "Charlie take(s) the test"? In model generation, should we get [take, retake] as separate propositions
    , arguments = ["the test"]
    , predProps = [Repetition]
    })

charlieHasTakenTest = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "has taken" -- TODO model perfect aspect better (also below)
    , arguments = ["the test"]
    , predProps = []
    })

charlieHasNotTakenTest = Prop {
    content = Event {
        agent = "Charlie"
        , predicate = "has taken"
        , arguments = ["the test"]
        , predProps = []
    }
    , negated = True
    , cancellable = False
    }

charliePassTest = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "pass"
    , arguments = ["the test"]
    , predProps = []
    })

charlieNotPassTest = Prop {
    content = Event {
        agent = "Charlie"
        , predicate = "pass"
        , arguments = ["the test"]
        , predProps = []
    }
    , negated = True
    , cancellable = False
    }

charlieFailTest = propWithDefaults (Event {
    agent = "Charlie"
    , predicate = "fail"
    , arguments = ["the test"]
    , predProps = []
    })

charlieNotFailTest = Prop {
    content = Event {
        agent = "Charlie"
        , predicate = "fail"
        , arguments = ["the test"]
        , predProps = []
    }
    , negated = True
    , cancellable = False
    }

-------------------------
-- PARSED PROPOSITIONS --
-------------------------

charliePassPast = ParsedProp {
    prop = charliePassTest
    , time = Past
    , mood = Indicative
    , presuppositions = [ charlieTakeTest ]
    , tree = charliePassedTheTest
    }

charlieNotPassPast = ParsedProp {
    prop = charlieNotPassTest
    , time = Past
    , mood = Indicative
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieDidNotPassTheTest
    }

charlieFailPast = ParsedProp {
    prop = charlieFailTest
    , time = Past
    , mood = Indicative
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieFailedTheTest
    }

charlieNotFailPast = ParsedProp {
    prop = charlieNotFailTest
    , time = Past
    , mood = Indicative
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieDidNotFailTheTest
    }

-- "Charlie brings his calculator tomorrow"
charlieBringCalculatorTomorrowIndicative = ParsedProp {
    prop = charlieBringCalculator
    , time = DeltaInDays 1
    , mood = Indicative
    , presuppositions = [ charlieOwnCalculator ]
    , tree = charlieBringsHisCalculatorTomorrow
    }

-- "Charlie takes the test tomorrow"
charlieTakeTestTomorrowIndicative = ParsedProp {
    prop = charlieTakeTest
    , time = DeltaInDays 1
    , mood = Indicative
    , presuppositions = [ charlieHasNotTakenTest ]
    , tree = charlieTakesTheTestTomorrow
    }

-- "Charlie re-takes the test tomorrow"
charlieRetakeTestTomorrowIndicative = ParsedProp {
    prop = charlieRetakeTest
    , time = DeltaInDays 1
    , mood = Indicative
    , presuppositions = [ charlieHasTakenTest ]
    , tree = charlieRetakesTheTestTomorrow
    }

-- "Charlie will pass the test"
charliePassFutureIndicative = ParsedProp {
    prop = charliePassTest
    , time = Future
    , mood = Indicative
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWillPassTheTest
    }

-- "Charlie will not pass the test"
charlieNotPassFutureIndicative = ParsedProp {
    prop = charlieNotPassTest
    , time = Future
    , mood = Indicative
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWillNotPassTheTest
    }

-- "Charlie will fail the test"
charlieFailFutureIndicative = ParsedProp {
    prop = charlieFailTest
    , time = Future
    , mood = Indicative
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWillFailTheTest
    }

-- "Charlie will not fail the test"
charlieNotFailFutureIndicative = ParsedProp {
    prop = charlieNotFailTest
    , time = Future
    , mood = Indicative
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWillNotFailTheTest
    }

-- "[If] Charlie brought his calculator tomorrow"
charlieBringCalculatorTomorrowSubjunctive = ParsedProp {
    prop = charlieBringCalculator
    , time = DeltaInDays 1
    , mood = Indicative -- Technically the mood is indicative (or at least we can't tell) but this is the antecedent for subjunctive conditionals
    , presuppositions = [ charlieOwnCalculator ]
    , tree = charlieBroughtHisCalculatorTomorrow
    }

-- "[If] Charlie took the test tomorrow"
charlieTakeTestTomorrowSubjunctive = ParsedProp {
    prop = charlieTakeTest
    , time = DeltaInDays 1
    , mood = Indicative -- Technically (see above)
    , presuppositions = [ charlieHasNotTakenTest ]
    , tree = charlieTookTheTestTomorrow
    }

-- "[If] Charlie re-took the test tomorrow"
charlieRetakeTestTomorrowSubjunctive = ParsedProp {
    prop = charlieRetakeTest
    , time = DeltaInDays 1
    , mood = Indicative -- Technically (see above)
    , presuppositions = [ charlieHasTakenTest ]
    , tree = charlieRetookTheTestTomorrow
    }

-- "Charlie would pass the test"
charliePassFutureSubjunctive = ParsedProp {
    prop = charliePassTest
    , time = Future
    , mood = Subjunctive
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWouldPassTheTest
    }

-- "Charlie would not pass the test"
charlieNotPassFutureSubjunctive = ParsedProp {
    prop = charlieNotPassTest
    , time = Future
    , mood = Subjunctive
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWouldNotPassTheTest
    }

-- "Charlie would fail the test"
charlieFailFutureSubjunctive = ParsedProp {
    prop = charlieFailTest
    , time = Future
    , mood = Subjunctive
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWouldFailTheTest
    }

-- "Charlie would not fail the test"
charlieNotFailFutureSubjunctive = ParsedProp {
    prop = charlieNotFailTest
    , time = Future
    , mood = Subjunctive
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWouldNotFailTheTest
    }

-- "[If] Charlie had brought his calculator [when he took the test]"
charlieBringCalculatorPastCounterfactual = ParsedProp {
    prop = charlieBringCalculator
    , time = Past
    , mood = Indicative -- Technically the mood is indicative (or at least we can't tell) but this is the antecedent for subjunctive conditionals
    , presuppositions = [ charlieOwnCalculator ]
    , tree = charlieHadBroughtHisCalculator
    }

-- "[If] Charlie had taken the test yesterday"
charlieTakeTestYesterdayCounterfactual = ParsedProp {
    prop = charlieTakeTest
    , time = DeltaInDays (-1)
    , mood = Indicative -- Technically (see above)
    , presuppositions = [ charlieHasNotTakenTest ]
    , tree = charlieHadTakenTheTestYesterday
    }

-- "[If] Charlie had taken the test tomorrow"
charlieTakeTestTomorrowCounterfactual = ParsedProp {
    prop = charlieTakeTest
    , time = DeltaInDays 1
    , mood = Indicative -- Technically (see above)
    , presuppositions = [ charlieHasNotTakenTest ]
    , tree = charlieHadTakenTheTestTomorrow
    }

-- "Charlie would have passed the test"
charliePassCounterfactual = ParsedProp {
    prop = charliePassTest
    , time = PastOrPresent -- On its own, this prop doesn't have a time associated
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWouldHavePassedTheTest
    }

-- "Charlie would not have passed the test"
charlieNotPassCounterfactual = ParsedProp {
    prop = charlieNotPassTest
    , time = PastOrPresent -- On its own, this prop doesn't have a time associated
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWouldNotHavePassedTheTest
    }

-- "Charlie would have failed the test"
charlieFailCounterfactual = ParsedProp {
    prop = charlieFailTest
    , time = PastOrPresent -- On its own, this prop doesn't have a time associated
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWouldHaveFailedTheTest
    }

-- "Charlie wouldn't have failed the test"
charlieNotFailCounterfactual = ParsedProp {
    prop = charlieNotFailTest
    , time = PastOrPresent -- On its own, this prop doesn't have a time associated
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWouldNotHaveFailedTheTest
    }