module Grammar
    ( Sent , NP , VP , VPAdjunct
    , getVerbForms
    , getAdjuncts
    ) where

import Lexicon

-- Grammar
-- TODO this way of encoding conditionals is very brittle, relies on "if" being only C
data Sent = Sent1 NP VP | Sent2 C Sent Sent deriving Show
data NP = NP1 D N | NP2 ProperN deriving Show
data VP = VP1 V | VP2 V N | VP3 Neg VP | VP4 Aux VP | VP5 VP VPAdjunct deriving Show
data VPAdjunct = VPAdjunct AdverbialN deriving Show

-- Utility methods
getVerbForms :: Sent -> [VerbForm]
getVerbForms (Sent1 np vp) = getVerbFormsVP vp
getVerbForms (Sent2 c s1 s2) = getVerbForms s1 ++ getVerbForms s2

getVerbFormsVP :: VP -> [VerbForm]
getVerbFormsVP (VP1 v) = [vForm v]
getVerbFormsVP (VP2 v n) = [vForm v]
getVerbFormsVP (VP3 neg vp) = getVerbFormsVP vp
getVerbFormsVP (VP4 aux vp) = (auxForm aux) : (getVerbFormsVP vp)
getVerbFormsVP (VP5 vp adjunct) = getVerbFormsVP vp

getAdjuncts :: Sent -> [VPAdjunct]
getAdjuncts (Sent1 np vp) = getAdjunctsVP vp
getAdjuncts (Sent2 c s1 s2) = getAdjuncts s1 ++ getAdjuncts s2

getAdjunctsVP :: VP -> [VPAdjunct]
getAdjunctsVP (VP1 v) = []
getAdjunctsVP (VP2 v n) = []
getAdjunctsVP (VP3 neg vp) = getAdjunctsVP vp
getAdjunctsVP (VP4 aux vp) = getAdjunctsVP vp
getAdjunctsVP (VP5 vp adjunct) = (getAdjunctsVP vp) ++ [adjunct]