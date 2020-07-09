module Grammar.Semantics where

-- Operates on strings because predicates in Events in DataStructures.hs are just encoded as strings
-- Instead it should operate on predicate meanings
-- When adding new predicates, should always be symmetric
antonymPred :: String -> Maybe String
antonymPred "pass" = Just "fail"
antonymPred "fail" = Just "pass"
antonymPred _      = Nothing
