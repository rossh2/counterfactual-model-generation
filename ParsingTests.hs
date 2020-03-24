module ParsingTests where
-- TODO this should live in the test directory - for that to work, need to configure Haskell compiler to understand directories
-- https://stackoverflow.com/questions/6606045/haskell-ghci-loading-modules-from-different-directories
-- https://stackoverflow.com/questions/5034685/what-is-the-haskell-syntax-to-import-modules-in-subdirectories

import DataStructures
import Grammar
import ParsedExamples
import Parsing
import TreeExamples
import TypeClasses

equalExceptPresupps :: ParsedProp -> ParsedProp -> Bool
-- No need to check trees, and don't check presupp's for now because they're not implemented yet
equalExceptPresupps p1 p2 = prop p1 == prop p2 && time p1 == time p2 && mood p1 == mood p2

sentEqualExceptPresupps :: ParsedSentence -> Maybe ParsedSentence -> Bool
sentEqualExceptPresupps s Nothing = False
sentEqualExceptPresupps (ParsedSimpleSentence p1) (Just (ParsedSimpleSentence p2)) = equalExceptPresupps p1 p2
sentEqualExceptPresupps (ParsedConditional p1 q1 timeC1 tree1) (Just (ParsedConditional p2 q2 timeC2 tree2)) =
    equalExceptPresupps p1 p2 && equalExceptPresupps q1 q2 && timeC1 == timeC2
sentEqualExceptPresupps _ _ = False

parseFromExample :: ParsedSentence -> Maybe ParsedSentence
parseFromExample sent@(ParsedSimpleSentence parsedP) = parseSentence (tree parsedP) []
parseFromExample sent@(ParsedConditional p q timeC sentTree) = parseSentence sentTree stressedWords
    where stressedWords = if timeC then extractTemporalAdjuncts (tree p) else []

extractTemporalAdjuncts :: SentTree -> [String]
-- Extract temporal adjuncts to stress from sentence or from antecedent of conditional
extractTemporalAdjuncts (Sent1 (TP np vp)) = getTemporalAdjunctHeads vp
extractTemporalAdjuncts (Sent2 c (TP np vp) tp2) = getTemporalAdjunctHeads vp

checkExampleEqual :: ParsedSentence -> Bool
checkExampleEqual sent = sentEqualExceptPresupps sent (parseFromExample sent)

-- Tests
tested = map (\x -> (x, checkExampleEqual x)) allSentences
testResults = map snd tested
allTests = all snd tested

-- Diagnostics
noParse = map ((\x -> x == Nothing) . parseFromExample) allSentences
parsedButIncorrectExists = not (map snd tested == map not noParse)

failingSentences = map (showLin . fst) (filter (not . snd) tested)

printSideBySide :: ParsedSentence -> IO ()
printSideBySide sent = do
    let showParsed = case parseFromExample sent of
                    Just parsedSent -> showExceptTreeAndPresupps parsedSent
                    Nothing -> "Nothing"
    putStrLn ("Expected:\n" ++ showExceptTreeAndPresupps sent ++ "\n\nActual:\n" ++ showParsed)

showExceptTreeAndPresupps :: ParsedSentence -> String
showExceptTreeAndPresupps (ParsedSimpleSentence p) = "ParsedSimpleSentence { pProp = " ++ showPropExcept p ++ "}"
showExceptTreeAndPresupps (ParsedConditional p q tc sentTree) = "ParsedConditional { antecedent = " ++ showPropExcept p
    ++ ", consequent = " ++ showPropExcept q ++ ", timeContrast = " ++ show tc ++ "}"

showPropExcept :: ParsedProp -> String
showPropExcept p = "ParsedProp { prop = " ++ show (prop p) ++ ", time = " ++ show (time p) ++ ", mood = " ++ show (mood p) ++ "}"
