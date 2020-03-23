module Lexicon where

import Features
import Times
import TypeClasses


-- This could be extended to have subcat frames on verbs easily, but would need something to validate them
data N = N { nHead :: String, nRoot :: String, nNumber :: SgPl } deriving (Show, Eq)
data ProperN = ProperN { properNHead :: String, properNNumber :: SgPl } deriving (Show, Eq)
data AdverbialN = AdverbialN { advNHead :: String, advTime :: Time } deriving (Show, Eq)
data Adverb = Adverb { advHead :: String } deriving (Show, Eq)
data D = D { dHead :: String } deriving (Show, Eq)
data V = V { vHead :: String, vRoot :: String, vForm :: VerbForm, predicateProps :: [PredicateProps] } deriving (Show, Eq)
data Aux = Aux { auxHead :: String, auxRoot :: String, auxEffect :: AuxEffect, auxForm :: VerbForm } deriving (Show, Eq)
data Neg = Neg { negHead :: String } deriving (Show, Eq)
data C = C { cHead :: String } deriving (Show, Eq)

instance ShowLinear N where
    showLin n = nHead n
instance ShowLinear ProperN where
    showLin pN = properNHead pN
instance ShowLinear AdverbialN where
    showLin advN = advNHead advN
instance ShowLinear Adverb where
    showLin adv = advHead adv
instance ShowLinear D where
    showLin d = dHead d
instance ShowLinear V where
    showLin v = vHead v
instance ShowLinear Aux where
    showLin aux = auxHead aux
instance ShowLinear Neg where
    showLin neg = negHead neg
instance ShowLinear C where
    showLin c = cHead c


charlie = ProperN { properNHead = "Charlie", properNNumber = Sg }

calculator = N { nHead = "calculator", nRoot = "calculator", nNumber = Sg }
calculators = N { nHead = "calculators", nRoot = "calculator", nNumber = Pl }
test = N { nHead = "test", nRoot = "test", nNumber = Sg }
tests = N { nHead = "tests", nRoot = "test", nNumber = Pl }

tomorrow = AdverbialN { advNHead = "tomorrow", advTime = DeltaInDays 1 }
yesterday = AdverbialN { advNHead = "yesterday", advTime = DeltaInDays (-1) }

quickly = Adverb { advHead = "quickly" }

a = D { dHead = "a" }
his = D { dHead = "his" }
the = D { dHead = "the" }

own = V { vHead = "own", vRoot = "own", vForm = PresentTense, predicateProps = [] }
owns = V { vHead = "owns", vRoot = "own", vForm = Present3Sg, predicateProps = [] }
owned = V { vHead = "owned", vRoot = "own", vForm = PastTense, predicateProps = [] }
ownedEn = V { vHead = "owned", vRoot = "own", vForm = EnEd, predicateProps = [] }
ownInf = V { vHead = "own", vRoot = "own", vForm = Inf, predicateProps = [] }
bring = V { vHead = "bring", vRoot = "bring", vForm = PresentTense, predicateProps = [] }
brings = V { vHead = "brings", vRoot = "bring", vForm = Present3Sg, predicateProps = [] }
brought = V { vHead = "brought", vRoot = "bring", vForm = PastTense, predicateProps = [] }
broughtEn = V { vHead = "brought", vRoot = "bring", vForm = EnEd, predicateProps = [] }
bringInf = V { vHead = "bring", vRoot = "bring", vForm = Inf, predicateProps = [] }
take = V { vHead = "take", vRoot = "take", vForm = PresentTense, predicateProps = [] }
takes = V { vHead = "takes", vRoot = "take", vForm = Present3Sg, predicateProps = [] }
took = V { vHead = "took", vRoot = "take", vForm = PastTense, predicateProps = [] }
taken = V { vHead = "taken", vRoot = "take", vForm = EnEd , predicateProps = []}
takeInf = V { vHead = "take", vRoot = "take", vForm = Inf, predicateProps = [] }
retake = V { vHead = "retake", vRoot = "retake", vForm = PresentTense, predicateProps = [Repetition] }
retakes = V { vHead = "retakes", vRoot = "retake", vForm = Present3Sg, predicateProps = [Repetition] }
retook = V { vHead = "retook", vRoot = "retake", vForm = PastTense, predicateProps = [Repetition] }
retaken = V { vHead = "retaken", vRoot = "retake", vForm = EnEd, predicateProps = [Repetition] }
retakeInf = V { vHead = "retake", vRoot = "retake", vForm = Inf, predicateProps = [Repetition] }
pass = V { vHead = "pass", vRoot = "pass", vForm = PresentTense, predicateProps = [] }
passes = V { vHead = "passes", vRoot = "pass", vForm = Present3Sg, predicateProps = [] }
passed = V { vHead = "passed", vRoot = "pass", vForm = PastTense, predicateProps = [] }
passedEn = V { vHead = "passed", vRoot = "pass", vForm = EnEd, predicateProps = [] }
passInf = V { vHead = "pass", vRoot = "pass", vForm = Inf, predicateProps = [] }
failV = V { vHead = "fail", vRoot = "fail", vForm = PresentTense, predicateProps = [] } -- "fail" a keyword in Haskell/Prelude
fails = V { vHead = "fails", vRoot = "fail", vForm = Present3Sg, predicateProps = [] }
failed = V { vHead = "failed", vRoot = "fail", vForm = PastTense, predicateProps = [] }
failedEn = V { vHead = "failed", vRoot = "fail", vForm = EnEd, predicateProps = [] }
failInf = V { vHead = "fail", vRoot = "fail", vForm = Inf, predicateProps = [] }

-- The tenses for these, and whether they in fact have the same head e.g. "woll", are unclear and not relevant to this problem
will = Aux { auxHead = "will", auxRoot = "will", auxEffect = FutureEffect, auxForm = Modal }
would = Aux { auxHead = "would", auxRoot = "would", auxEffect = Possible, auxForm = Modal }
doPres = Aux { auxHead = "do", auxRoot = "do", auxEffect = NoEffect, auxForm = PresentTense }
does = Aux { auxHead = "does", auxRoot = "do", auxEffect = NoEffect, auxForm = Present3Sg }
did = Aux { auxHead = "did", auxRoot = "do", auxEffect = NoEffect, auxForm = PastTense }
done = Aux { auxHead = "done", auxRoot = "do", auxEffect = NoEffect, auxForm = EnEd }
have = Aux { auxHead = "have", auxRoot = "have", auxEffect = Perfect, auxForm = PresentTense }
has = Aux { auxHead = "has", auxRoot = "have", auxEffect = Perfect, auxForm = Present3Sg }
had = Aux { auxHead = "had", auxRoot = "have", auxEffect = Perfect, auxForm = PastTense }
haveInf = Aux { auxHead = "have", auxRoot = "have", auxEffect = Perfect, auxForm = Inf }

notNeg = Neg { negHead = "not" } -- "not" is a Haskell keyword

ifC = C { cHead = "if" } -- "if" is a Haskell keyword
-- TODO do we need e.g. then or silent then? What is the standard syntactic way for modelling conditionals?
