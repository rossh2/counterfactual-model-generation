module Test.TreeExamples where

import Grammar.Grammar
import Grammar.Lexicon

----------------------
-- Simple Sentences --
----------------------

charlieBringsHisCalculatorTomorrow = (Sent1 (TP (NP2 charlie) (VP5 (VP2 brings (NP1 his calculator)) (VPA1 tomorrow))))
charlieBroughtHisCalculatorTomorrow = (Sent1 (TP (NP2 charlie) (VP5 (VP2 brought (NP1 his calculator)) (VPA1 tomorrow))))
charlieHadBroughtHisCalculatorTomorrow = (Sent1 (TP (NP2 charlie) (VP5 (VP4 had (VP2 broughtEn (NP1 his calculator))) (VPA1 tomorrow))))
charlieHadBroughtHisCalculatorYesterday = (Sent1 (TP (NP2 charlie) (VP5 (VP4 had (VP2 broughtEn (NP1 his calculator))) (VPA1 yesterday))))

charlieTakesTheTestTomorrow = (Sent1 (TP (NP2 charlie) (VP5 (VP2 takes (NP1 the test)) (VPA1 tomorrow))))
charlieTookTheTestTomorrow = (Sent1 (TP (NP2 charlie) (VP5 (VP2 took (NP1 the test)) (VPA1 tomorrow))))
charlieHadTakenTheTestTomorrow = (Sent1 (TP (NP2 charlie) (VP5 (VP4 had (VP2 taken (NP1 the test))) (VPA1 tomorrow))))
charlieHadTakenTheTestYesterday = (Sent1 (TP (NP2 charlie) (VP5 (VP4 had (VP2 taken (NP1 the test))) (VPA1 yesterday))))

charlieRetakesTheTestTomorrow = (Sent1 (TP (NP2 charlie) (VP5 (VP2 retakes (NP1 the test)) (VPA1 tomorrow))))
charlieRetookTheTestTomorrow = (Sent1 (TP (NP2 charlie) (VP5 (VP2 retook (NP1 the test)) (VPA1 tomorrow))))

charliePassedTheTest = (Sent1 (TP (NP2 charlie) (VP2 passed (NP1 the test))))
charlieWillPassTheTest = (Sent1 (TP (NP2 charlie) (VP4 will (VP2 passInf (NP1 the test)))))
charlieWouldPassTheTest = (Sent1 (TP (NP2 charlie) (VP4 would (VP2 passInf (NP1 the test)))))
charlieWouldHavePassedTheTest = (Sent1 (TP (NP2 charlie) (VP4 would (VP4 haveInf (VP2 passedEn (NP1 the test))))))

charlieFailedTheTest = (Sent1 (TP (NP2 charlie) (VP2 failed (NP1 the test))))
charlieWillFailTheTest = (Sent1 (TP (NP2 charlie) (VP4 will (VP2 failInf (NP1 the test)))))
charlieWouldFailTheTest = (Sent1 (TP (NP2 charlie) (VP4 would (VP2 failInf (NP1 the test)))))
charlieWouldHaveFailedTheTest = (Sent1 (TP (NP2 charlie) (VP4 would (VP4 haveInf (VP2 failedEn (NP1 the test))))))

charlieDidNotFailTheTest = (Sent1 (TP (NP2 charlie) (VP4 did (VP3 notNeg (VP2 failInf (NP1 the test))))))
charlieWouldNotFailTheTest = (Sent1 (TP (NP2 charlie) (VP4 would (VP3 notNeg (VP2 failInf (NP1 the test))))))
charlieWillNotFailTheTest = (Sent1 (TP (NP2 charlie) (VP4 will (VP3 notNeg (VP2 failInf (NP1 the test))))))
charlieWouldNotHaveFailedTheTest = (Sent1 (TP (NP2 charlie) (VP4 would (VP3 notNeg (VP4 haveInf (VP2 failedEn (NP1 the test)))))))


------------------
-- Conditionals --
------------------

extractTP :: SentTree -> TP
-- This is only meant to be called on simple sentences
extractTP (Sent1 tp) = tp
extractTP (Sent2 c tp1 tp2) = tp1 -- dummy implementation

-- Indicative
ifCharlieBringsHisCalculatorTomorrowCharlieWillPass = (Sent2 ifC
    (extractTP charlieBringsHisCalculatorTomorrow)
    (extractTP charlieWillPassTheTest))
ifCharlieBringsHisCalculatorTomorrowCharlieWillNotFail = (Sent2 ifC
    (extractTP charlieBringsHisCalculatorTomorrow)
    (extractTP charlieWillNotFailTheTest))
ifCharlieTakesTheTestTomorrowCharlieWillPass = (Sent2 ifC
    (extractTP charlieTakesTheTestTomorrow)
    (extractTP charlieWillPassTheTest))
ifCharlieTakesTheTestTomorrowCharlieWillNotFail = (Sent2 ifC
    (extractTP charlieTakesTheTestTomorrow)
    (extractTP charlieWillNotFailTheTest))
ifCharlieRetakesTheTestTomorrowCharlieWillPass = (Sent2 ifC
    (extractTP charlieRetakesTheTestTomorrow)
    (extractTP charlieWillPassTheTest))
ifCharlieRetakesTheTestTomorrowCharlieWillNotFail = (Sent2 ifC
    (extractTP charlieRetakesTheTestTomorrow)
    (extractTP charlieWillNotFailTheTest))

-- Subjunctive
ifCharlieBroughtHisCalculatorTomorrowCharlieWouldPass = (Sent2 ifC
    (extractTP charlieBroughtHisCalculatorTomorrow)
    (extractTP charlieWouldPassTheTest))
ifCharlieBroughtHisCalculatorTomorrowCharlieWouldNotFail = (Sent2 ifC
    (extractTP charlieBroughtHisCalculatorTomorrow)
    (extractTP charlieWouldNotFailTheTest))
ifCharlieTookTheTestTomorrowCharlieWouldPass = (Sent2 ifC
    (extractTP charlieTookTheTestTomorrow)
    (extractTP charlieWouldPassTheTest))
ifCharlieTookTheTestTomorrowCharlieWouldNotFail = (Sent2 ifC
    (extractTP charlieTookTheTestTomorrow)
    (extractTP charlieWouldNotFailTheTest))
ifCharlieRetookTheTestTomorrowCharlieWouldPass = (Sent2 ifC
    (extractTP charlieRetookTheTestTomorrow)
    (extractTP charlieWouldPassTheTest))
ifCharlieRetookTheTestTomorrowCharlieWillNotFail = (Sent2 ifC
    (extractTP charlieRetookTheTestTomorrow)
    (extractTP charlieWouldNotFailTheTest))

-- Counterfactual
ifCharlieHadBroughtHisCalculatorYesterdayCharlieWouldHavePassed = (Sent2 ifC
    (extractTP charlieHadBroughtHisCalculatorYesterday)
    (extractTP charlieWouldHavePassedTheTest))
ifCharlieHadBroughtHisCalculatorYesterdayCharlieWouldNotHaveFailed = (Sent2 ifC
    (extractTP charlieHadBroughtHisCalculatorYesterday)
    (extractTP charlieWouldNotHaveFailedTheTest))
ifCharlieHadTakenTheTestYesterdayCharlieWouldHavePassed = (Sent2 ifC
    (extractTP charlieHadTakenTheTestYesterday)
    (extractTP charlieWouldHavePassedTheTest))
ifCharlieHadTakenTheTestYesterdayCharlieWouldNotHaveFailed = (Sent2 ifC
    (extractTP charlieHadTakenTheTestYesterday)
    (extractTP charlieWouldNotHaveFailedTheTest))
ifCharlieHadTakenTheTestTomorrowCharlieWouldHavePassed = (Sent2 ifC
    (extractTP charlieHadTakenTheTestTomorrow)
    (extractTP charlieWouldHavePassedTheTest))
ifCharlieHadTakenTheTestTomorrowCharlieWouldNotHaveFailed = (Sent2 ifC
    (extractTP charlieHadTakenTheTestTomorrow)
    (extractTP charlieWouldNotHaveFailedTheTest))
