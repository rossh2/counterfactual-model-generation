module Test.ParsingTests where

import Grammar.Grammar
import Parsing.DataStructures
import Parsing.Parsing
import Test.ParsedSentenceExamples
import Test.TreeExamples
import Test.TestUtils

main :: IO ()
main = do
    let tested = testExamples allSentences
    printCorrectCountOrDebugSingle tested

testExamples :: [ParsedSentence] -> [(ParsedSentence, Bool)]
testExamples = map (\x -> (x, checkExampleEqual x))

checkExampleEqual :: ParsedSentence -> Bool
checkExampleEqual sent = sentEqualExceptPresupps sent (parseFromExample sent)

sentEqualExceptPresupps :: ParsedSentence -> Maybe ParsedSentence -> Bool
sentEqualExceptPresupps s Nothing = False
sentEqualExceptPresupps (ParsedSimpleSentence p1) (Just (ParsedSimpleSentence p2)) = equalExceptPresupps p1 p2
sentEqualExceptPresupps (ParsedConditional p1 q1 tree1) (Just (ParsedConditional p2 q2 tree2)) =
    equalExceptPresupps p1 p2 && equalExceptPresupps q1 q2
sentEqualExceptPresupps _ _ = False

equalExceptPresupps :: ParsedProp -> ParsedProp -> Bool
-- No need to check trees, and don't check presupp's for now because they're not implemented yet
equalExceptPresupps p1 p2 = prop p1 == prop p2 && mood p1 == mood p2

parseFromExample :: ParsedSentence -> Maybe ParsedSentence
parseFromExample sent@(ParsedSimpleSentence parsedP) = parseSentence (tree parsedP)
parseFromExample sent@(ParsedConditional p q sentTree) = parseSentence sentTree

-----------------
-- Diagnostics --
-----------------

printSideBySide :: ParsedSentence -> IO ()
printSideBySide sent = do
    let showParsed = case parseFromExample sent of
                    Just parsedSent -> showExceptTreeAndPresupps parsedSent
                    Nothing -> "Nothing"
    putStrLn ("Expected:\n" ++ showExceptTreeAndPresupps sent ++ "\n\nActual:\n" ++ showParsed)

showExceptTreeAndPresupps :: ParsedSentence -> String
showExceptTreeAndPresupps (ParsedSimpleSentence p) = "ParsedSimpleSentence { pProp = " ++ showPropExcept p ++ "}"
showExceptTreeAndPresupps (ParsedConditional p q sentTree) = "ParsedConditional { antecedent = " ++ showPropExcept p
    ++ ", consequent = " ++ showPropExcept q ++ "}"

showPropExcept :: ParsedProp -> String
showPropExcept p = "ParsedProp { prop = " ++ show (prop p) ++ ", mood = " ++ show (mood p) ++ "}"
