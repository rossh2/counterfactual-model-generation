module Test.ParsedSentenceExamples where

import Grammar.Features
import Model.Times
import Parsing.DataStructures
import Test.PropExamples
import Test.TreeExamples

---------------
-- SENTENCES --
---------------

passPastSentence = ParsedSimpleSentence {
    pProp = charliePassPast
    }

notPassPastSentence = ParsedSimpleSentence {
    pProp = charlieNotPassPast
    }

failPastSentence = ParsedSimpleSentence {
    pProp = charlieFailPast
    }

notFailPastSentence = ParsedSimpleSentence {
    pProp = charlieNotFailPast
    }

simpleSentences = [
    passPastSentence
    , notPassPastSentence
    , failPastSentence
    , notFailPastSentence
    ]

------------------
-- CONDITIONALS --
------------------

-- "If Charlie brings his calculator tomorrow, Charlie will pass the test."
indicativePassConditional = ParsedConditional {
    antecedent = charlieBringCalculatorTomorrowIndicative
    , consequent = charliePassFutureIndicative
    , timeContrast = False
    , fullTree = ifCharlieBringsHisCalculatorTomorrowCharlieWillPass
    }

-- "If Charlie brings his calculator tomorrow, Charlie will not pass the test."
indicativeNotPassConditional = ParsedConditional {
    antecedent = charlieBringCalculatorTomorrowIndicative
    , consequent = charlieNotPassFutureIndicative
    , timeContrast = False
    , fullTree = ifCharlieBringsHisCalculatorTomorrowCharlieWillNotPass
    }

-- "If Charlie brings his calculator tomorrow, Charlie will fail the test."
indicativeFailConditional = ParsedConditional {
    antecedent = charlieBringCalculatorTomorrowIndicative
    , consequent = charlieFailFutureIndicative
    , timeContrast = False
    , fullTree = ifCharlieBringsHisCalculatorTomorrowCharlieWillFail
    }

-- "If Charlie brings his calculator tomorrow, Charlie will not fail the test."
indicativeNotFailConditional = ParsedConditional {
    antecedent = charlieBringCalculatorTomorrowIndicative
    , consequent = charlieNotFailFutureIndicative
    , timeContrast = False
    , fullTree = ifCharlieBringsHisCalculatorTomorrowCharlieWillNotFail
    }

-- "If Charlie takes the test tomorrow, Charlie will pass the test."
indicativeNoFocusPassConditional = ParsedConditional {
    antecedent = charlieTakeTestTomorrowIndicative
    , consequent = charliePassFutureIndicative
    , timeContrast = False
    , fullTree = ifCharlieTakesTheTestTomorrowCharlieWillPass
    }

-- "If Charlie takes the test tomorrow, Charlie will not pass the test."
indicativeNoFocusNotPassConditional = ParsedConditional {
    antecedent = charlieTakeTestTomorrowIndicative
    , consequent = charlieNotPassFutureIndicative
    , timeContrast = False
    , fullTree = ifCharlieTakesTheTestTomorrowCharlieWillNotPass
    }

-- "If Charlie takes the test tomorrow, Charlie will fail the test."
indicativeNoFocusFailConditional = ParsedConditional {
    antecedent = charlieTakeTestTomorrowIndicative
    , consequent = charlieFailFutureIndicative
    , timeContrast = False
    , fullTree = ifCharlieTakesTheTestTomorrowCharlieWillFail
    }

-- "If Charlie takes the test tomorrow, Charlie will not fail the test."
indicativeNoFocusNotFailConditional = ParsedConditional {
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

-- "If Charlie re-takes the test *tomorrow*, Charlie will not pass the test."
indicativeTimeFocusNotPassConditional = ParsedConditional {
    antecedent = charlieRetakeTestTomorrowIndicative
    , consequent = charlieNotPassFutureIndicative
    , timeContrast = True
    , fullTree = ifCharlieRetakesTheTestTomorrowCharlieWillNotPass
    }

-- "If Charlie re-takes the test *tomorrow*, Charlie will fail the test."
indicativeTimeFocusFailConditional = ParsedConditional {
    antecedent = charlieRetakeTestTomorrowIndicative
    , consequent = charlieFailFutureIndicative
    , timeContrast = True
    , fullTree = ifCharlieRetakesTheTestTomorrowCharlieWillFail
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

-- "If Charlie brought his calculator tomorrow, Charlie would not pass the test."
subjunctiveNotPassConditional = ParsedConditional {
    antecedent = charlieBringCalculatorTomorrowSubjunctive
    , consequent = charlieNotPassFutureSubjunctive
    , timeContrast = False
    , fullTree = ifCharlieBroughtHisCalculatorTomorrowCharlieWouldNotPass
    }

-- "If Charlie brought his calculator tomorrow, Charlie would fail the test."
subjunctiveFailConditional = ParsedConditional {
    antecedent = charlieBringCalculatorTomorrowSubjunctive
    , consequent = charlieFailFutureSubjunctive
    , timeContrast = False
    , fullTree = ifCharlieBroughtHisCalculatorTomorrowCharlieWouldFail
    }

-- "If Charlie brought his calculator tomorrow, Charlie would not fail the test."
subjunctiveNotFailConditional = ParsedConditional {
    antecedent = charlieBringCalculatorTomorrowSubjunctive
    , consequent = charlieNotFailFutureSubjunctive
    , timeContrast = False
    , fullTree = ifCharlieBroughtHisCalculatorTomorrowCharlieWouldNotFail
    }

-- "If Charlie took the test tomorrow, Charlie would pass the test."
subjunctiveNoFocusPassConditional = ParsedConditional {
    antecedent = charlieTakeTestTomorrowSubjunctive
    , consequent = charliePassFutureSubjunctive
    , timeContrast = False
    , fullTree = ifCharlieTookTheTestTomorrowCharlieWouldPass
    }

-- "If Charlie took the test tomorrow, Charlie would not pass the test."
subjunctiveNoFocusNotPassConditional = ParsedConditional {
    antecedent = charlieTakeTestTomorrowSubjunctive
    , consequent = charlieNotPassFutureSubjunctive
    , timeContrast = False
    , fullTree = ifCharlieTookTheTestTomorrowCharlieWouldNotPass
    }

-- "If Charlie took the test tomorrow, Charlie would fail the test."
subjunctiveNoFocusFailConditional = ParsedConditional {
    antecedent = charlieTakeTestTomorrowSubjunctive
    , consequent = charlieFailFutureSubjunctive
    , timeContrast = False
    , fullTree = ifCharlieTookTheTestTomorrowCharlieWouldFail
    }

-- "If Charlie took the test tomorrow, Charlie wouldn't fail the test."
subjunctiveNoFocusNotFailConditional = ParsedConditional {
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

-- "If Charlie re-took the test *tomorrow*, Charlie would not pass the test."
subjunctiveTimeFocusNotPassConditional = ParsedConditional {
    antecedent = charlieRetakeTestTomorrowSubjunctive
    , consequent = charlieNotPassFutureSubjunctive
    , timeContrast = True
    , fullTree = ifCharlieRetookTheTestTomorrowCharlieWouldNotPass
    }

-- "If Charlie re-took the test *tomorrow*, Charlie would fail the test."
subjunctiveTimeFocusFailConditional = ParsedConditional {
    antecedent = charlieRetakeTestTomorrowSubjunctive
    , consequent = charlieFailFutureSubjunctive
    , timeContrast = True
    , fullTree = ifCharlieRetookTheTestTomorrowCharlieWouldFail
    }

-- "If Charlie re-took the test *tomorrow*, Charlie wouldn't fail the test."
subjunctiveTimeFocusNotFailConditional = ParsedConditional {
    antecedent = charlieRetakeTestTomorrowSubjunctive
    , consequent = charlieNotFailFutureSubjunctive
    , timeContrast = True
    , fullTree = ifCharlieRetookTheTestTomorrowCharlieWouldNotFail
    }

-- "If Charlie had brought his calculator, Charlie would have passed the test."
passCounterfactual = ParsedConditional {
    antecedent = charlieBringCalculatorPastCounterfactual
    , consequent = charliePassCounterfactual
    , timeContrast = False
    , fullTree = ifCharlieHadBroughtHisCalculatorCharlieWouldHavePassed
    }

-- "If Charlie had brought his calculator, Charlie would not have passed the test."
notPassCounterfactual = ParsedConditional {
    antecedent = charlieBringCalculatorPastCounterfactual
    , consequent = charlieNotPassCounterfactual
    , timeContrast = False
    , fullTree = ifCharlieHadBroughtHisCalculatorCharlieWouldNotHavePassed
    }

-- "If Charlie had brought his calculator, Charlie would have failed the test."
failCounterfactual = ParsedConditional {
    antecedent = charlieBringCalculatorPastCounterfactual
    , consequent = charlieFailCounterfactual
    , timeContrast = False
    , fullTree = ifCharlieHadBroughtHisCalculatorCharlieWouldHaveFailed
    }

-- "If Charlie had brought his calculator yesterday, Charlie wouldn't have failed the test."
notFailCounterfactual = ParsedConditional {
    antecedent = charlieBringCalculatorPastCounterfactual
    , consequent = charlieNotFailCounterfactual
    , timeContrast = False
    , fullTree = ifCharlieHadBroughtHisCalculatorCharlieWouldNotHaveFailed
    }

-- "If Charlie had taken the test *yesterday*, Charlie would have passed the test."
timeFocusPassCounterfactual = ParsedConditional {
    antecedent = charlieTakeTestYesterdayCounterfactual
    , consequent = charliePassCounterfactual
    , timeContrast = True
    , fullTree = ifCharlieHadTakenTheTestYesterdayCharlieWouldHavePassed
    }

-- "If Charlie had taken the test *yesterday*, Charlie would not have passed the test."
timeFocusNotPassCounterfactual = ParsedConditional {
    antecedent = charlieTakeTestYesterdayCounterfactual
    , consequent = charlieNotPassCounterfactual
    , timeContrast = True
    , fullTree = ifCharlieHadTakenTheTestYesterdayCharlieWouldNotHavePassed
    }

-- "If Charlie had taken the test *yesterday*, Charlie would have failed the test."
timeFocusFailCounterfactual = ParsedConditional {
    antecedent = charlieTakeTestYesterdayCounterfactual
    , consequent = charlieFailCounterfactual
    , timeContrast = True
    , fullTree = ifCharlieHadTakenTheTestYesterdayCharlieWouldHaveFailed
    }

-- "If Charlie had taken the test *yesterday*, Charlie wouldn't have failed the test."
timeFocusNotFailCounterfactual = ParsedConditional {
    antecedent = charlieTakeTestYesterdayCounterfactual
    , consequent = charlieNotFailCounterfactual
    , timeContrast = True
    , fullTree = ifCharlieHadTakenTheTestYesterdayCharlieWouldNotHaveFailed
    }

-- "If Charlie had taken the test *tomorrow*, Charlie would have passed the test."
futurePassCounterfactual = ParsedConditional {
    antecedent = charlieTakeTestTomorrowCounterfactual
    , consequent = charliePassCounterfactual
    , timeContrast = True
    , fullTree = ifCharlieHadTakenTheTestTomorrowCharlieWouldHavePassed
    }

-- "If Charlie had taken the test *tomorrow*, Charlie would not have passed the test."
futureNotPassCounterfactual = ParsedConditional {
    antecedent = charlieTakeTestTomorrowCounterfactual
    , consequent = charlieNotPassCounterfactual
    , timeContrast = True
    , fullTree = ifCharlieHadTakenTheTestTomorrowCharlieWouldNotHavePassed
    }

-- "If Charlie had taken the test *tomorrow*, Charlie would have failed the test."
futureFailCounterfactual = ParsedConditional {
    antecedent = charlieTakeTestTomorrowCounterfactual
    , consequent = charlieFailCounterfactual
    , timeContrast = True
    , fullTree = ifCharlieHadTakenTheTestTomorrowCharlieWouldHaveFailed
    }

-- "If Charlie had taken the test *tomorrow*, Charlie would not have failed the test."
futureNotFailCounterfactual = ParsedConditional {
    antecedent = charlieTakeTestTomorrowCounterfactual
    , consequent = charlieNotFailCounterfactual
    , timeContrast = True
    , fullTree = ifCharlieHadTakenTheTestTomorrowCharlieWouldNotHaveFailed
    }

---------------------------
-- Lists of conditionals --
---------------------------

indicativeConditionals = [
    indicativePassConditional
    , indicativeNotPassConditional
    , indicativeFailConditional
    , indicativeNotFailConditional
    ]
    
indicativeNoFocusConditionals = [
    indicativeNoFocusPassConditional
    , indicativeNoFocusNotPassConditional
    , indicativeNoFocusFailConditional
    , indicativeNoFocusNotFailConditional
    ]

indicativeTimeFocusConditionals = [
    indicativeTimeFocusPassConditional
    , indicativeTimeFocusNotPassConditional
    , indicativeTimeFocusFailConditional
    , indicativeTimeFocusNotFailConditional
    ]

subjunctiveConditionals = [
    subjunctivePassConditional
    , subjunctiveNotPassConditional
    , subjunctiveFailConditional
    , subjunctiveNotFailConditional
    ]
        
subjunctiveNoFocusConditionals = [
    subjunctiveNoFocusPassConditional
    , subjunctiveNoFocusNotPassConditional
    , subjunctiveNoFocusFailConditional
    , subjunctiveNoFocusNotFailConditional
    ]

subjunctiveTimeFocusConditionals = [
    subjunctiveTimeFocusPassConditional
    , subjunctiveTimeFocusNotPassConditional
    , subjunctiveTimeFocusFailConditional
    , subjunctiveTimeFocusNotFailConditional
    ]

    
counterfactuals = [
    passCounterfactual
    , notPassCounterfactual
    , failCounterfactual
    , notFailCounterfactual 
    ]

timeFocusCounterfactuals = [
    timeFocusPassCounterfactual
    , timeFocusNotPassCounterfactual
    , timeFocusFailCounterfactual
    , timeFocusNotFailCounterfactual
    ]

futureCounterfactuals = [
    futurePassCounterfactual
    , futureNotPassCounterfactual
    , futureFailCounterfactual
    , futureNotFailCounterfactual
    ]

allConditionals = indicativeConditionals
    ++ indicativeNoFocusConditionals
    ++ indicativeTimeFocusConditionals
    ++ subjunctiveConditionals
    ++ subjunctiveNoFocusConditionals
    ++ subjunctiveTimeFocusConditionals
    ++ counterfactuals
    ++ timeFocusCounterfactuals
    ++ futureCounterfactuals

allSentences = simpleSentences
    ++ allConditionals

-- A selection of conditionals sorted by pass/fail, for printing

standardPassConditionals = [
    indicativePassConditional
    , subjunctivePassConditional
    , passCounterfactual
    , indicativeNoFocusPassConditional
    , subjunctiveNoFocusPassConditional
    ]

standardNotFailConditionals = [
    indicativeNotFailConditional
    , subjunctiveNotFailConditional
    , notFailCounterfactual
    , indicativeNoFocusNotFailConditional
    , subjunctiveNoFocusNotFailConditional
    ]

timeContrastPassConditionals = [
    indicativeTimeFocusPassConditional
    , subjunctiveTimeFocusPassConditional
    , timeFocusPassCounterfactual
    , futurePassCounterfactual
    ]

timeContrastNotFailConditionals = [
    indicativeTimeFocusNotFailConditional
    , subjunctiveTimeFocusNotFailConditional
    , timeFocusNotFailCounterfactual
    , futureNotFailCounterfactual
    ]

----------------------
--    Discourses    --
----------------------

----------------------
-- Simple sentences --
----------------------

passPassSimple = [passPastSentence, passPastSentence]
passNotPassSimple = [passPastSentence, notPassPastSentence]
passFailSimple = [passPastSentence, failPastSentence]
passNotFailSimple = [passPastSentence, notFailPastSentence]

notPassPassSimple = [notPassPastSentence, passPastSentence]
notPassNotPassSimple = [notPassPastSentence, notPassPastSentence]
notPassFailSimple = [notPassPastSentence, failPastSentence]
notPassNotFailSimple = [notPassPastSentence, notFailPastSentence]

failPassSimple = [failPastSentence, passPastSentence]
failNotPassSimple = [failPastSentence, notPassPastSentence]
failFailSimple = [failPastSentence, failPastSentence]
failNotFailSimple = [failPastSentence, notFailPastSentence]

notFailPassSimple = [notFailPastSentence, passPastSentence]
notFailNotPassSimple = [notFailPastSentence, notPassPastSentence]
notFailFailSimple = [notFailPastSentence, failPastSentence]
notFailNotFailSimple = [notFailPastSentence, notFailPastSentence]


----------------------------------------
-- Counterfactuals without time focus --
----------------------------------------

passPassNoFocusIndicative = [passPastSentence, indicativePassConditional]
passPassNoFocusSubjunctive = [passPastSentence, subjunctivePassConditional]

passPassNoFocusCounterfactual = [passPastSentence, passCounterfactual]
passNotPassNoFocusCounterfactual = [passPastSentence, notPassCounterfactual]
passFailNoFocusCounterfactual = [passPastSentence, failCounterfactual]
passNotFailNoFocusCounterfactual = [passPastSentence, notFailCounterfactual]
notPassPassNoFocusCounterfactual = [notPassPastSentence, passCounterfactual]
notPassNotPassNoFocusCounterfactual = [notPassPastSentence, notPassCounterfactual]
notPassFailNoFocusCounterfactual = [notPassPastSentence, failCounterfactual]
notPassNotFailNoFocusCounterfactual = [notPassPastSentence, notFailCounterfactual]
failPassNoFocusCounterfactual = [failPastSentence, passCounterfactual]
failNotPassNoFocusCounterfactual = [failPastSentence, notPassCounterfactual]
failFailNoFocusCounterfactual = [failPastSentence, failCounterfactual]
failNotFailNoFocusCounterfactual = [failPastSentence, notFailCounterfactual]
notFailPassNoFocusCounterfactual = [notFailPastSentence, passCounterfactual]
notFailNotPassNoFocusCounterfactual = [notFailPastSentence, notPassCounterfactual]
notFailFailNoFocusCounterfactual = [notFailPastSentence, failCounterfactual]
notFailNotFailNoFocusCounterfactual = [notFailPastSentence, notFailCounterfactual]

-------------------------------------
--   Conditionals with time focus  --
-------------------------------------

passPassIndicative = [passPastSentence, indicativeTimeFocusPassConditional]
passPassSubjunctive = [passPastSentence, subjunctiveTimeFocusPassConditional]
passPassCounterfactual = [passPastSentence, timeFocusPassCounterfactual]

passNotPassIndicative = [passPastSentence, indicativeTimeFocusNotPassConditional]
passNotPassSubjunctive = [passPastSentence, subjunctiveTimeFocusNotPassConditional]
passNotPassCounterfactual = [passPastSentence, timeFocusNotPassCounterfactual]

passFailIndicative = [passPastSentence, indicativeTimeFocusFailConditional]
passFailSubjunctive = [passPastSentence, subjunctiveTimeFocusFailConditional]
passFailCounterfactual = [passPastSentence, timeFocusFailCounterfactual]

passNotFailIndicative = [passPastSentence, indicativeTimeFocusNotFailConditional]
passNotFailSubjunctive = [passPastSentence, subjunctiveTimeFocusNotFailConditional]
passNotFailCounterfactual = [passPastSentence, timeFocusNotFailCounterfactual]

notPassPassIndicative = [notPassPastSentence, indicativeTimeFocusPassConditional]
notPassPassSubjunctive = [notPassPastSentence, subjunctiveTimeFocusPassConditional]
notPassPassCounterfactual = [notPassPastSentence, timeFocusPassCounterfactual]

notPassNotPassIndicative = [notPassPastSentence, indicativeTimeFocusNotPassConditional]
notPassNotPassSubjunctive = [notPassPastSentence, subjunctiveTimeFocusNotPassConditional]
notPassNotPassCounterfactual = [notPassPastSentence, timeFocusNotPassCounterfactual]

notPassFailIndicative = [notPassPastSentence, indicativeTimeFocusFailConditional]
notPassFailSubjunctive = [notPassPastSentence, subjunctiveTimeFocusFailConditional]
notPassFailCounterfactual = [notPassPastSentence, timeFocusFailCounterfactual]

notPassNotFailIndicative = [notPassPastSentence, indicativeTimeFocusNotFailConditional]
notPassNotFailSubjunctive = [notPassPastSentence, subjunctiveTimeFocusNotFailConditional]
notPassNotFailCounterfactual = [notPassPastSentence, timeFocusNotFailCounterfactual]

failPassIndicative = [failPastSentence, indicativeTimeFocusPassConditional]
failPassSubjunctive = [failPastSentence, subjunctiveTimeFocusPassConditional]
failPassCounterfactual = [failPastSentence, timeFocusPassCounterfactual]

failNotPassIndicative = [failPastSentence, indicativeTimeFocusNotPassConditional]
failNotPassSubjunctive = [failPastSentence, subjunctiveTimeFocusNotPassConditional]
failNotPassCounterfactual = [failPastSentence, timeFocusNotPassCounterfactual]

failFailIndicative = [failPastSentence, indicativeTimeFocusFailConditional]
failFailSubjunctive = [failPastSentence, subjunctiveTimeFocusFailConditional]
failFailCounterfactual = [failPastSentence, timeFocusFailCounterfactual]

failNotFailIndicative = [failPastSentence, indicativeTimeFocusNotFailConditional]
failNotFailSubjunctive = [failPastSentence, subjunctiveTimeFocusNotFailConditional]
failNotFailCounterfactual = [failPastSentence, timeFocusNotFailCounterfactual]

notFailPassIndicative = [notFailPastSentence, indicativeTimeFocusPassConditional]
notFailPassSubjunctive = [notFailPastSentence, subjunctiveTimeFocusPassConditional]
notFailPassCounterfactual = [notFailPastSentence, timeFocusPassCounterfactual]

notFailNotPassIndicative = [notFailPastSentence, indicativeTimeFocusNotPassConditional]
notFailNotPassSubjunctive = [notFailPastSentence, subjunctiveTimeFocusNotPassConditional]
notFailNotPassCounterfactual = [notFailPastSentence, timeFocusNotPassCounterfactual]

notFailFailIndicative = [notFailPastSentence, indicativeTimeFocusFailConditional]
notFailFailSubjunctive = [notFailPastSentence, subjunctiveTimeFocusFailConditional]
notFailFailCounterfactual = [notFailPastSentence, timeFocusFailCounterfactual]

notFailNotFailIndicative = [notFailPastSentence, indicativeTimeFocusNotFailConditional]
notFailNotFailSubjunctive = [notFailPastSentence, subjunctiveTimeFocusNotFailConditional]
notFailNotFailCounterfactual = [notFailPastSentence, timeFocusNotFailCounterfactual]

-- Some additional future counterfactuals

passNotFailFutureCounterfactual = [passPastSentence, futureNotFailCounterfactual]
failNotFailFutureCounterfactual = [failPastSentence, futureNotFailCounterfactual]
notPassNotFailFutureCounterfactual = [notPassPastSentence, futureNotFailCounterfactual]
notFailNotFailFutureCounterfactual = [notFailPastSentence, futureNotFailCounterfactual]

------------------------------------------
-- Lists of some interesting discourses --
-- TODO        (incomplete)             --
------------------------------------------

contradictionDiscourses = [
    passNotPassSimple
    , failNotFailSimple
    , passFailSimple
    ]

passNotFailDiscourses = [
    passNotFailIndicative
    , passNotFailSubjunctive
    , passNotFailCounterfactual
    , passNotFailFutureCounterfactual
    , passNotFailNoFocusCounterfactual
    ]

notPassNotFailDiscourses = [
    notPassNotFailIndicative
    , notPassNotFailSubjunctive
    , notPassNotFailCounterfactual
    , notPassNotFailFutureCounterfactual
    , notPassNotFailNoFocusCounterfactual
    ]

failNotFailDiscourses = [
    failNotFailIndicative
    , failNotFailSubjunctive
    , failNotFailCounterfactual
    , failNotFailFutureCounterfactual
    , failNotFailNoFocusCounterfactual
    ]

notFailNotFailDiscourses = [
    notFailNotFailIndicative
    , notFailNotFailSubjunctive
    , notFailNotFailCounterfactual
    , notFailNotFailFutureCounterfactual
    , notFailNotFailNoFocusCounterfactual
    ]

allDiscourses = contradictionDiscourses
    ++ passNotFailDiscourses
    ++ notPassNotFailDiscourses
    ++ failNotFailDiscourses
    ++ notFailNotFailDiscourses
