module Lexicon where

import Time

-- Let's not deal with the verb 'be' here which distinguishes extra forms
data VerbForm = Present | Present3Sg | Past | EnEd | Inf | Modal deriving Show
data SgPl = Sg | Pl deriving Show
data AuxEffect = Future | Perfect | Possible | None deriving Show

-- this could be extended to have subcat frames on verbs easily, but would need something to validate them
data N = { nHead :: String, number :: SgPl } deriving Show
data ProperN = { properNHead :: String, number :: SgPl } deriving Show
data AdverbialN = { advNHead :: String, time :: Time }
data D = { dHead :: String } deriving Show
data V = { vHead :: String, vForm :: VerbForm } deriving Show
data Aux = { auxHead :: String, auxEffect :: AuxEffect, auxForm :: VerbForm } deriving Show
data Neg = { negHead :: String } deriving Show
data C = { cHead :: String } deriving Show

charlie = ProperN { properNHead = "Charlie", number = Sg }

calculator = N { nHead = "calculator", number = Sg }
calculators = N { nHead = "calculator", number = Pl }
test = N { nHead = "test", number = Sg }
tests = N { nHead = "test", number = Pl }

tomorrow = AdverbialN { advNHead = "tomorrow", time = DeltaInDays 1 }
yesterday = AdverbialN { advNHead = "yesterday", time = DeltaInDays (-1) }

a = D { dHead = "a" }
his = D { dHead = "his" }

own = V { vHead = "own", vForm = Present }
owns = V { vHead = "own", vForm = Present3Sg }
owned = V { vHead = "own", vForm = Past }
ownedEn = V { vHead = "own", vForm = EnEd }
ownInf = V { vHead = "own", vForm = Inf }
bring = V { vHead = "bring", vForm = Present }
brings = V { vHead = "bring", vForm = Present3Sg }
brought = V { vHead = "bring", vForm = Past }
broughtEn = V { vHead = "bring", vForm = EnEd }
bringInf = V { vHead = "bring", vForm = Inf }
take = V { vHead = "take", vForm = Present }
takes = V { vHead = "take", vForm = Present3Sg }
took = V { vHead = "take", vForm = Past }
taken = V { vHead = "take", vForm = EnEd }
takeInf = V { vHead = "take", vForm = Inf }
pass = V { vHead = "pass", vForm = Present }
passes = V { vHead = "pass", vForm = Present3Sg }
passed = V { vHead = "pass", vForm = Past }
passedEn = V { vHead = "pass", vForm = EnEd }
passInf = V { vHead = "pass", vForm = Inf }
fail = V { vHead = "fail", vForm = Present }
fails = V { vHead = "fail", vForm = Present3Sg }
failed = V { vHead = "fail", vForm = Past }
failedEn = V { vHead = "fail", vForm = EnEd }
failInf = V { vHead = "fail", vForm = Inf }

-- The tenses for these, and whether they in fact have the same head e.g. "woll", are unclear and not relevant to this problem
will = Aux { auxHead = "will", auxEffect = Future, auxForm = Modal }
would = Aux { auxHead = "would", auxEffect = Possible, auxForm = Modal }
doPres = Aux { auxHead = "do", auxEffect = None, auxForm = Present }
does = Aux { auxHead = "do", auxEffect = None, auxForm = Present3sg }
did = Aux { auxHead = "do", auxEffect = None, auxForm = Past }
done = Aux { auxHead = "do", auxEffect = None, auxForm = EnEd }
have = Aux { auxHead = "have", auxEffect = Perfect, auxForm = Present }
has = Aux { auxHead = "have", auxEffect = Perfect, auxForm = Present3sg }
had = Aux { auxHead = "have", auxEffect = Perfect, auxForm = Past }

not = Neg { negHead = "not" }

ifC = C { cHead = "if" } -- obviously "if" is a Haskell keyword
-- TODO do we need these?
thenC = C { cHead = "then" }
silentThen = C { cHead = "" }