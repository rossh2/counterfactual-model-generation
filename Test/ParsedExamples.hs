module Test.ParsedExamples where

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

-- "Charlie won't fail the test"
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

-- "Charlie wouldn't fail the test"
charlieNotFailFutureSubjunctive = ParsedProp {
    prop = charlieNotFailTest
    , time = Future
    , mood = Subjunctive
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWouldNotFailTheTest
    }

-- "[If] Charlie had brought his calculator yesterday"
charlieBringCalculatorYesterdayCounterfactual = ParsedProp {
    prop = charlieBringCalculator
    , time = DeltaInDays (-1)
    , mood = Indicative -- Technically the mood is indicative (or at least we can't tell) but this is the antecedent for subjunctive conditionals
    , presuppositions = [ charlieOwnCalculator ]
    , tree = charlieHadBroughtHisCalculatorYesterday
    }

-- "[If] Charlie had brought his calculator tomorrow"
charlieBringCalculatorTomorrowCounterfactual = ParsedProp {
    prop = charlieBringCalculator
    , time = DeltaInDays 1
    , mood = Indicative -- Technically the mood is indicative (or at least we can't tell) but this is the antecedent for subjunctive conditionals
    , presuppositions = [ charlieOwnCalculator ]
    , tree = charlieHadBroughtHisCalculatorTomorrow
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

-- "Charlie wouldn't have failed the test"
charlieNotFailCounterfactual = ParsedProp {
    prop = charlieNotFailTest
    , time = PastOrPresent -- On its own, this prop doesn't have a time associated
    , mood = Counterfactual
    , presuppositions = [ charlieTakeTest ]
    , tree = charlieWouldNotHaveFailedTheTest
    }

--------------------------------
-- SENTENCES AND CONDITIONALS --
--------------------------------

passPastSentence = ParsedSimpleSentence {
    pProp = charliePassPast
    }

failPastSentence = ParsedSimpleSentence {
    pProp = charlieFailPast
    }

notFailPastSentence = ParsedSimpleSentence {
    pProp = charlieNotFailPast
    }

-- "If Charlie brings his calculator tomorrow, Charlie will pass the test."
indicativePassConditional = ParsedConditional {
    antecedent = charlieBringCalculatorTomorrowIndicative
    , consequent = charliePassFutureIndicative
    , timeContrast = False
    , fullTree = ifCharlieBringsHisCalculatorTomorrowCharlieWillPass
    }

-- "If Charlie brings his calculator tomorrow, Charlie won't fail the test."
indicativeNotFailConditional = ParsedConditional {
    antecedent = charlieBringCalculatorTomorrowIndicative
    , consequent = charlieNotFailFutureIndicative
    , timeContrast = False
    , fullTree = ifCharlieBringsHisCalculatorTomorrowCharlieWillNotFail
    }

-- "If Charlie takes the test tomorrow, Charlie will pass the test."
indicativeTimeNoFocusPassConditional = ParsedConditional {
    antecedent = charlieTakeTestTomorrowIndicative
    , consequent = charliePassFutureIndicative
    , timeContrast = False
    , fullTree = ifCharlieTakesTheTestTomorrowCharlieWillPass
    }

-- "If Charlie takes the test tomorrow, Charlie won't fail the test."
indicativeTimeNoFocusNotFailConditional = ParsedConditional {
    antecedent = charlieTakeTestTomorrowIndicative
    , consequent = charlieNotFailFutureIndicative
    , timeContrast = False
    , fullTree = ifCharlieTakesTheTestTomorrowCharlieWillNotFail
    }

-- "If Charlie re-takes the test *tomorrow*, Charlie will pass the test."
indicativeTimeFocusPassConditional = ParsedConditional {
    antecedent = charlieRetakeTestTomorrowIndicative
    , consequent = charliePassFutureIndicative
    , timeContrast = True
    , fullTree = ifCharlieRetakesTheTestTomorrowCharlieWillPass
    }

-- "If Charlie re-takes the test *tomorrow*, Charlie won't fail the test."
indicativeTimeFocusNotFailConditional = ParsedConditional {
    antecedent = charlieRetakeTestTomorrowIndicative
    , consequent = charlieNotFailFutureIndicative
    , timeContrast = True
    , fullTree = ifCharlieRetakesTheTestTomorrowCharlieWillNotFail
    }

-- "If Charlie brought his calculator tomorrow, Charlie would pass the test."
subjunctivePassConditional = ParsedConditional {
    antecedent = charlieBringCalculatorTomorrowSubjunctive
    , consequent = charliePassFutureSubjunctive
    , timeContrast = False
    , fullTree = ifCharlieBroughtHisCalculatorTomorrowCharlieWouldPass
    }

-- "If Charlie brought his calculator tomorrow, Charlie wouldn't fail the test."
subjunctiveNotFailConditional = ParsedConditional {
    antecedent = charlieBringCalculatorTomorrowSubjunctive
    , consequent = charlieNotFailFutureSubjunctive
    , timeContrast = False
    , fullTree = ifCharlieBroughtHisCalculatorTomorrowCharlieWouldNotFail
    }

-- "If Charlie took the test tomorrow, Charlie would pass the test."
subjunctiveTimeNoFocusPassConditional = ParsedConditional {
    antecedent = charlieTakeTestTomorrowSubjunctive
    , consequent = charliePassFutureSubjunctive
    , timeContrast = False
    , fullTree = ifCharlieTookTheTestTomorrowCharlieWouldPass
    }

-- "If Charlie took the test tomorrow, Charlie wouldn't fail the test."
subjunctiveTimeNoFocusNotFailConditional = ParsedConditional {
    antecedent = charlieTakeTestTomorrowSubjunctive
    , consequent = charlieNotFailFutureSubjunctive
    , timeContrast = False
    , fullTree = ifCharlieTookTheTestTomorrowCharlieWouldNotFail
    }

-- "If Charlie re-took the test *tomorrow*, Charlie would pass the test."
subjunctiveTimeFocusPassConditional = ParsedConditional {
    antecedent = charlieRetakeTestTomorrowSubjunctive
    , consequent = charliePassFutureSubjunctive
    , timeContrast = True
    , fullTree = ifCharlieRetookTheTestTomorrowCharlieWouldPass
    }

-- "If Charlie re-took the test *tomorrow*, Charlie wouldn't fail the test."
subjunctiveTimeFocusNotFailConditional = ParsedConditional {
    antecedent = charlieRetakeTestTomorrowSubjunctive
    , consequent = charlieNotFailFutureSubjunctive
    , timeContrast = True
    , fullTree = ifCharlieRetookTheTestTomorrowCharlieWillNotFail
    }

-- "If Charlie had brought his calculator yesterday, Charlie would have passed the test."
pastPassCounterfactual = ParsedConditional {
    antecedent = charlieBringCalculatorYesterdayCounterfactual
    , consequent = charliePassCounterfactual
    , timeContrast = False
    , fullTree = ifCharlieHadBroughtHisCalculatorYesterdayCharlieWouldHavePassed
    }

-- "If Charlie had brought his calculator yesterday, Charlie wouldn't have failed the test."
pastNotFailCounterfactual = ParsedConditional {
    antecedent = charlieBringCalculatorYesterdayCounterfactual
    , consequent = charlieNotFailCounterfactual
    , timeContrast = False
    , fullTree = ifCharlieHadBroughtHisCalculatorYesterdayCharlieWouldNotHaveFailed
    }

-- "If Charlie had taken the test *yesterday*, Charlie would have passed the test."
pastTimeFocusPassCounterfactual = ParsedConditional {
    antecedent = charlieTakeTestYesterdayCounterfactual
    , consequent = charliePassCounterfactual
    , timeContrast = True
    , fullTree = ifCharlieHadTakenTheTestYesterdayCharlieWouldHavePassed
    }

-- "If Charlie had taken the test *yesterday*, Charlie wouldn't have failed the test."
pastTimeFocusNotFailCounterfactual = ParsedConditional {
    antecedent = charlieTakeTestYesterdayCounterfactual
    , consequent = charlieNotFailCounterfactual
    , timeContrast = True
    , fullTree = ifCharlieHadTakenTheTestYesterdayCharlieWouldNotHaveFailed
    }

-- "If Charlie had taken the test *tomorrow*, Charlie would have passed the test."
futureTimeFocusPassCounterfactual = ParsedConditional {
    antecedent = charlieTakeTestTomorrowCounterfactual
    , consequent = charliePassCounterfactual
    , timeContrast = True
    , fullTree = ifCharlieHadTakenTheTestTomorrowCharlieWouldHavePassed
    }

-- "If Charlie had taken the test *tomorrow*, Charlie wouldn't have failed the test."
futureTimeFocusNotFailCounterfactual = ParsedConditional {
    antecedent = charlieTakeTestTomorrowCounterfactual
    , consequent = charlieNotFailCounterfactual
    , timeContrast = True
    , fullTree = ifCharlieHadTakenTheTestTomorrowCharlieWouldNotHaveFailed
    }

simpleSentences = [
    passPastSentence
    , failPastSentence
    , notFailPastSentence
    ]

standardPassConditionals = [
    indicativePassConditional
    , subjunctivePassConditional
    , pastPassCounterfactual
    , indicativeTimeNoFocusPassConditional
    , subjunctiveTimeNoFocusPassConditional
    ]

standardNotFailConditionals = [
    indicativeNotFailConditional
    , subjunctiveNotFailConditional
    , pastNotFailCounterfactual
    , indicativeTimeNoFocusNotFailConditional
    , subjunctiveTimeNoFocusNotFailConditional
    ]

timeContrastPassConditionals = [
    indicativeTimeFocusPassConditional
    , subjunctiveTimeFocusPassConditional
    , pastTimeFocusPassCounterfactual
    , futureTimeFocusPassCounterfactual
    ]

timeContrastNotFailConditionals = [
    indicativeTimeFocusNotFailConditional
    , subjunctiveTimeFocusNotFailConditional
    , pastTimeFocusNotFailCounterfactual
    , futureTimeFocusNotFailCounterfactual
    ]

allSentences = simpleSentences
    ++ standardPassConditionals
    ++ standardNotFailConditionals
    ++ timeContrastPassConditionals
    ++ timeContrastNotFailConditionals

----------------
-- Discourses --
----------------

failNotFailSimple = [failPastSentence, notFailPastSentence]
passFailSimple = [passPastSentence, failPastSentence]

failNotFailIndicative = [failPastSentence, indicativeTimeFocusNotFailConditional]
failNotFailSubjunctive = [failPastSentence, subjunctiveTimeFocusNotFailConditional]
failNotFailPastCounterfactual = [failPastSentence, pastTimeFocusNotFailCounterfactual]
failNotFailFutureCounterfactual = [failPastSentence, futureTimeFocusNotFailCounterfactual]

notFailNotFailIndicative = [notFailPastSentence, indicativeTimeFocusNotFailConditional]
notFailNotFailSubjunctive = [notFailPastSentence, subjunctiveTimeFocusNotFailConditional]
notFailNotFailPastCounterfactual = [notFailPastSentence, pastTimeFocusNotFailCounterfactual]
notFailNotFailFutureCounterfactual = [notFailPastSentence, futureTimeFocusNotFailCounterfactual]

passNotFailIndicative = [passPastSentence, indicativeTimeFocusNotFailConditional]
passNotFailSubjunctive = [passPastSentence, subjunctiveTimeFocusNotFailConditional]
passNotFailPastCounterfactual = [passPastSentence, pastTimeFocusNotFailCounterfactual]
passNotFailFutureCounterfactual = [passPastSentence, futureTimeFocusNotFailCounterfactual]

contradictionDiscourses = [
    failNotFailSimple
    , passFailSimple
    ]

failNotFailDiscourses = [
    failNotFailIndicative
    , failNotFailSubjunctive
    , failNotFailPastCounterfactual
    , failNotFailFutureCounterfactual
    ]

notFailNotFailDiscourses = [
    notFailNotFailIndicative
    , notFailNotFailSubjunctive
    , notFailNotFailPastCounterfactual
    , notFailNotFailFutureCounterfactual
    ]

passNotFailDiscourses = [
    passNotFailIndicative
    , passNotFailSubjunctive
    , passNotFailPastCounterfactual
    , passNotFailFutureCounterfactual
    ]

allDiscourses = contradictionDiscourses
    ++ failNotFailDiscourses
    ++ notFailNotFailDiscourses
    ++ passNotFailDiscourses