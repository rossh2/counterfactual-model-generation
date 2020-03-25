module Grammar.Grammar where

import qualified Data.Char as Char

import Grammar.Features
import Grammar.Lexicon
import Utils.TypeClasses

-- A note on conditionals:
-- In the literature, the conditional if-clause is modelled as a CP starting in VP-adjunct position and moving to the
-- front of the sentence. This is motivated by the alternative construction "Charlie would have passed his test if he had taken it yesterday."
-- Because we need to model the final sentence (S-Structure), we model it simply as C TP TP.
-- Arguably more accurate would be to have it recursive: CP -> C TP | CP TP, where the C is usually silent or may be "then".
-- However this would allow e.g. CP -> CP TP -> (CP TP) TP -> ((C TP) TP) TP i.e. "if Charlie laughed, (then) Mary laughed, (then) John laughed."
-- which isn't really grammatical.

data SentTree = Sent1 TP | Sent2 C TP TP deriving (Show, Eq)
data TP = TP NP VP deriving (Show, Eq)
data NP = NP1 D N | NP2 ProperN deriving (Show, Eq)
data VP = VP1 V | VP2 V NP | VP3 Neg VP | VP4 Aux VP | VP5 VP VPAdjunct deriving (Show, Eq)
data VPAdjunct = VPA1 AdverbialN | VPA2 Adverb deriving (Show, Eq)

instance ShowLinear SentTree where
    showLin (Sent1 tp) = capitalizeFirstLetter (showLin tp) ++ "."
    showLin (Sent2 c tp1 tp2) = capitalizeFirstLetter (showLin c) ++ " " ++ showLin tp1 ++ ", " ++ showLin tp2 ++ "."
    showLinList [] = ""
    showLinList (x:xs) = showLin x ++ " " ++ showLinList xs
instance ShowLinear TP where
    showLin (TP np vp) = showLin np ++ " " ++ showLin vp
instance ShowLinear NP where
    showLin (NP1 d n) = showLin d ++ " " ++ showLin n
    showLin (NP2 pn) = showLin pn
instance ShowLinear VP where
    showLin (VP1 v) = showLin v
    showLin (VP2 v n) = showLin v ++ " " ++ showLin n
    showLin (VP3 neg vp) = showLin neg ++ " " ++ showLin vp
    showLin (VP4 aux vp) = showLin aux ++ " " ++ showLin vp
    showLin (VP5 vp adj) = showLin vp ++ " " ++ showLin adj
instance ShowLinear VPAdjunct where
    showLin (VPA1 advN) = showLin advN
    showLin (VPA2 adv) = showLin adv

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter [] = []
capitalizeFirstLetter (c:cs) = Char.toUpper c : cs

-- Utility methods over recursive VPs and adjuncts
getRootV :: VP -> V
getRootV (VP1 v) = v
getRootV (VP2 v np) = v
getRootV (VP3 neg vp) = getRootV vp
getRootV (VP4 aux vp) = getRootV vp
getRootV (VP5 vp adjunct) = getRootV vp

isVPNegated :: VP -> Bool
isVPNegated (VP1 v) = False
isVPNegated (VP2 v np) = False
isVPNegated (VP3 neg vp) = not (isVPNegated vp) -- Use simple classical logic for double negations
isVPNegated (VP4 aux vp) = isVPNegated vp
isVPNegated (VP5 vp adjunct) = isVPNegated vp

getVerbForms :: VP -> [VerbForm]
getVerbForms (VP1 v) = [vForm v]
getVerbForms (VP2 v np) = [vForm v]
getVerbForms (VP3 neg vp) = getVerbForms vp
getVerbForms (VP4 aux vp) = auxForm aux : getVerbForms vp
getVerbForms (VP5 vp adjunct) = getVerbForms vp

getAuxEffects :: VP -> [AuxEffect]
getAuxEffects (VP1 v) = []
getAuxEffects (VP2 v np) = []
getAuxEffects (VP3 neg vp) = getAuxEffects vp
getAuxEffects (VP4 aux vp) = auxEffect aux : getAuxEffects vp
getAuxEffects (VP5 vp adjunct) = getAuxEffects vp

getAdjuncts :: VP -> [VPAdjunct]
getAdjuncts (VP1 v) = []
getAdjuncts (VP2 v np) = []
getAdjuncts (VP3 neg vp) = getAdjuncts vp
getAdjuncts (VP4 aux vp) = getAdjuncts vp
getAdjuncts (VP5 vp adjunct) = getAdjuncts vp ++ [adjunct]

getAdjunctHead :: VPAdjunct -> String
getAdjunctHead (VPA1 advN) = advNHead advN
getAdjunctHead (VPA2 adv) = advHead adv
