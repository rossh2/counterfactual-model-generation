module Main where

import Control.Applicative

import Model.ModelGeneration
import Parsing.DataStructures
import Test.ParsedExamples
import Utils.TypeClasses

-- This is outdated now that ModelGenerationTests exists.
-- Consider replacing with a UI where you can enter trees and return the model,
-- or pick from the test examples and show the model
main :: IO ()
main = do
    showSimpleSentences
    putStrLn ""
    showStandardConditionals
    putStrLn ""
    showTimeContrastConditionals
    putStrLn ""
    showDiscourses

showSimpleSentences :: IO ()
showSimpleSentences = do
    putStrLn "SIMPLE SENTENCES"
    putStrLn (showSentenceModels simpleSentences)
    putStrLn ""

showStandardConditionals :: IO ()
showStandardConditionals = do
    putStrLn "STANDARD CONDITIONALS - PASS"
    putStrLn (showSentenceModels standardPassConditionals)
    putStrLn ""
    putStrLn "STANDARD CONDITIONALS - NOT FAIL"
    putStrLn (showSentenceModels standardNotFailConditionals)
    putStrLn ""

showTimeContrastConditionals :: IO ()
showTimeContrastConditionals = do
    putStrLn "CONDITIONALS WITH FOCUSED TIME / EVENT REPETITION - PASS"
    putStrLn (showSentenceModels timeContrastPassConditionals)
    putStrLn ""
    putStrLn "CONDITIONALS WITH FOCUSED TIME / EVENT REPETITION - NOT FAIL"
    putStrLn (showSentenceModels timeContrastNotFailConditionals)
    putStrLn ""

showDiscourses :: IO ()
showDiscourses = do
    putStrLn "TWO-SENTENCE DISCOUSES - CONTRADICTIONS"
    putStrLn (showDiscourseModels contradictionDiscourses)
    putStrLn "TWO-SENTENCE DISCOURSES - FAIL vs. NOT FAIL"
    putStrLn (showDiscourseModels failNotFailDiscourses)
    putStrLn ""
    putStrLn "TWO-SENTENCE DISCOURSES - NOT FAIL vs. NOT FAIL"
    putStrLn (showDiscourseModels notFailNotFailDiscourses)
    putStrLn ""
    putStrLn "TWO-SENTENCE DISCOURSES - PASS vs. NOT FAIL"
    putStrLn (showDiscourseModels passNotFailDiscourses)

showSentenceModels :: [ParsedSentence] -> String
showSentenceModels conditionals = unlines $ conditionals <**> [showLin, showLin . generateModel]

showDiscourseModels :: [ParsedDiscourse] -> String
showDiscourseModels discourses = unlines $ discourses <**> [showLinList, showLin . generateDiscourseModel]
