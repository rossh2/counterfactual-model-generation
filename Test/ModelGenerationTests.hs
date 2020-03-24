module Test.ModelGenerationTests where

import Model.ModelGeneration
import Parsing.DataStructures
import Test.ParsedSentenceExamples

main :: IO ()
main = do
    putStrLn "** INDIVIDUAL SENTENCES **"
    putStrLn "SIMPLE SENTENCES"
    putStrLn "Charlie __."
    putStrLn $ showCorrectCount singleSentencesExpected
    putStrLn "CONDITIONALS"
    putStrLn "If Charlie __, Charlie __."
    putStrLn $ showCorrectCount singleConditionalsExpected
    putStrLn "\n** DISCOURSES **"
    putStrLn "SIMPLE SENTENCES"
    putStrLn "Charlie __ the test. Charlie __ the test."
    putStrLn $ makeResultsTable simpleSentenceExpected
    putStrLn "INDICATIVE (NO TIME FOCUS - REPEATING UNIQUE EVENT)"
    putStrLn "Charlie __ the test. If Charlie takes the test tomorrow, Charlie will __ the test."
    putStrLn $ makeResultsList indicativeExpected
    putStrLn "SUBJUNCTIVE (NO TIME FOCUS - REPEATING UNIQUE EVENT)"
    putStrLn "Charlie __ the test. If Charlie took the test tomorrow, Charlie would __ the test."
    putStrLn $ makeResultsList subjunctiveExpected
    putStrLn "COUNTERFACTUAL (NO TIME FOCUS)"
    putStrLn "Charlie __ the test. If Charlie had brought his calculator, Charlie would have __ the test."
    putStrLn $ makeResultsTable counterfactualExpected
    putStrLn "INDICATIVE, TIME FOCUS"
    putStrLn "Charlie __ the test. If Charlie retakes the test tomorrow, Charlie will __ the test."
    putStrLn $ makeResultsTable indicativeTimeFocusExpected
    putStrLn "SUBJUNCTIVE, TIME FOCUS"
    putStrLn "Charlie __ the test. If Charlie retook the test tomorrow, Charlie would __ the test."
    putStrLn $ makeResultsTable subjunctiveTimeFocusExpected
    putStrLn "COUNTERFACTUAL, TIME FOCUS"
    putStrLn "Charlie __ the test. If Charlie had taken the test tomorrow, Charlie would have __ the test."
    putStrLn $ makeResultsTable counterfactualTimeFocusExpected

makeResultsList :: [(ParsedDiscourse, Bool)] -> String
makeResultsList ds = preamble ++ "\n" ++ show isCorrectList
    where isCorrectList = map checkFelicityPredicted ds
          preamble = countCorrect isCorrectList

countCorrect :: [Bool] -> String
countCorrect isCorrectList = "Correctly predicted: " ++ (show correctCount) ++ "/" ++ (show allCount)
    where correctCount = length $ filter id isCorrectList
          allCount = length isCorrectList

showCorrectCount :: [(ParsedDiscourse, Bool)] -> String
showCorrectCount ds = countCorrect isCorrectList
    where isCorrectList = map checkFelicityPredicted ds

checkFelicityPredicted :: (ParsedDiscourse, Bool) -> Bool
checkFelicityPredicted (discourse, actualFelicity) = predictedFelicity == actualFelicity
    where predictedFelicity = Nothing /= generateDiscourseModel discourse

------------------------------
-- General model generation --
------------------------------

-- Assert that a model can be built for every discourse containing just a single sentence / conditional
singleSentencesExpected = map (\x -> ([x], True)) simpleSentences
singleConditionalsExpected = map (\x -> ([x], True)) allConditionals

---------------------
-- Specific models --
---------------------

-- TODO check a few specific cases

-- TODO check special cases where model is rescued by meaning of "didn't fail"

---------------------------------------------------------------
-- Discourse felicity judgements                             --
-- Each pair represents whether the discourse is felicitous, --
-- i.e. whether a model should be generated or not           --
---------------------------------------------------------------

tableEdges = ["pass", "fail", "not pass", "not fail"]

-- Shows results for each case of whether the model was correct or not
makeResultsTable :: [[(ParsedDiscourse, Bool)]] -> String
makeResultsTable table = preamble ++ "\n" ++ unlines (map unwords allRows)
    where isCorrectTable = map (map checkFelicityPredicted) table
          labelledTableRows = zip tableEdges $ map (map show) isCorrectTable
          stringTableRows = map (\(label, cells) -> label : cells) labelledTableRows
          allRows = map (map (padString 9)) $ ((" " : tableEdges) : stringTableRows)
          correctCount = length $ concat $ map (filter id) isCorrectTable
          allCount = (length tableEdges)^2
          preamble = "Correctly predicted: " ++ (show correctCount) ++ "/" ++ (show allCount)

padString :: Int -> String -> String
padString n xs = take n $ xs ++ repeat ' '

----------------------
-- Simple sentences --
----------------------

simpleSentenceExpected = [
    [
        (passPassSimple, True)
        , (passFailSimple, False) -- Charlie passed the test. Charlie failed the test.
        , (passNotPassSimple, False) -- Charlie passed the test. Charlie didn't pass the test.
        , (passNotFailSimple, True)
        ]
    , [
        (failPassSimple, False)
        , (failFailSimple, True)
        , (failNotPassSimple, True)
        , (failNotFailSimple, False) -- Charlie failed the test. Charlie didn't fail the test.
        ]
    , [
        (notPassPassSimple, False)
        , (notPassFailSimple, True)
        , (notPassNotPassSimple, True)
        , (notPassNotFailSimple, False)
        ]
    , [
        (notFailPassSimple, True)
        , (notFailFailSimple, False)
        , (notFailNotPassSimple, False)
        , (notFailNotFailSimple, True)
        ]
    ]

----------------------
--   Conditionals   --
----------------------

indicativeExpected = [
    (passPassNoFocusIndicative, False) -- Charlie passed the test. If Charlie takes the test tomorrow, Charlie will pass the test.
    ]

subjunctiveExpected = [
    (passPassNoFocusSubjunctive, False) -- Charlie passed the test. If Charlie took the test tomorrow, Charlie would pass the test.
    ]

counterfactualExpected = [
    -- TODO check judgements still hold without explicit mention of time - should still be the case, because counterfactual?
    [
        (passPassNoFocusCounterfactual, True) -- Charlie passed the test. If Charlie had brought his calculator, Charlie would have passed the test.
        , (passFailNoFocusCounterfactual, True)
        , (passNotPassNoFocusCounterfactual, True)
        , (passNotFailNoFocusCounterfactual, False)
        ]
    , [
        (failPassNoFocusCounterfactual, True)
        , (failFailNoFocusCounterfactual, True)
        , (failNotPassNoFocusCounterfactual, False)
        , (failNotFailNoFocusCounterfactual, True)
        ]
    , [
        (notPassPassNoFocusCounterfactual, True)
        , (notPassFailNoFocusCounterfactual, False)
        , (notPassNotPassNoFocusCounterfactual, True)
        , (notPassNotFailNoFocusCounterfactual, True)
        ]
    , [
        (notFailPassNoFocusCounterfactual, False)
        , (notFailFailNoFocusCounterfactual, True)
        , (notFailNotPassNoFocusCounterfactual, True)
        , (notFailNotFailNoFocusCounterfactual, True)
        ]
    ]

indicativeTimeFocusExpected = [
    -- TODO check judgements for indicative - is true that anything goes?
    [
        (passPassIndicative, True) -- Charlie passed the test. If Charlie retakes the test tomorrow, Charlie will pass the test.
        , (passFailIndicative, True) -- Charlie passed the test. If Charlie retakes the test tomorrow, Charlie will fail the test.
        , (passNotPassIndicative, True) -- Charlie passed the test. If Charlie retakes the test tomorrow, Charlie will not pass the test.
        , (passNotFailIndicative, True) -- Charlie passed the test. If Charlie retakes the test tomorrow, Charlie will not fail the test.
        ]
    , [
        (failPassIndicative, True) -- Charlie failed the test. If Charlie retakes the test tomorrow, Charlie will pass the test.
        , (failFailIndicative, True) -- Charlie failed the test. If Charlie retakes the test tomorrow, Charlie will fail the test.
        , (failNotPassIndicative, True) -- Charlie failed the test. If Charlie retakes the test tomorrow, Charlie will not pass the test.
        , (failNotFailIndicative, True) -- Charlie failed the test. If Charlie retakes the test tomorrow, Charlie will not fail the test.
        ]
    , [
        (notPassPassIndicative, True) -- Charlie did not pass the test. If Charlie retakes the test tomorrow, Charlie will pass the test.
        , (notPassFailIndicative, True) -- Charlie did not pass the test. If Charlie retakes the test tomorrow, Charlie will fail the test.
        , (notPassNotPassIndicative, True) -- Charlie did not pass the test. If Charlie retakes the test tomorrow, Charlie will not pass the test.
        , (notPassNotFailIndicative, True) -- Charlie did not pass the test. If Charlie retakes the test tomorrow, Charlie will not fail the test.
        ]
    , [
        (notFailPassIndicative, True) -- Charlie failed the test. If Charlie retakes the test tomorrow, Charlie will pass the test.
        , (notFailFailIndicative, True) -- Charlie failed the test. If Charlie retakes the test tomorrow, Charlie will fail the test.
        , (notFailNotPassIndicative, True) -- Charlie failed the test. If Charlie retakes the test tomorrow, Charlie will not pass the test.
        , (notFailNotFailIndicative, True) -- Charlie failed the test. If Charlie retakes the test tomorrow, Charlie will not fail the test.
        ]
    ]

subjunctiveTimeFocusExpected = [
    [
        (passPassSubjunctive, True) -- Charlie passed the test. If Charlie retook the test tomorrow, Charlie would pass the test.
        , (passFailSubjunctive, True) -- Charlie passed the test. If Charlie retook the test tomorrow, Charlie would fail the test.
        , (passNotPassSubjunctive, True) -- Charlie passed the test. If Charlie retook the test tomorrow, Charlie would not pass the test.
        , (passNotFailSubjunctive, True) -- Charlie passed the test. If Charlie retook the test tomorrow, Charlie would not fail the test.
        ]
    , [
        (failPassSubjunctive, True) -- Charlie failed the test. If Charlie retook the test tomorrow, Charlie would pass the test.
        , (failFailSubjunctive, True) -- Charlie failed the test. If Charlie retook the test tomorrow, Charlie would fail the test.
        , (failNotPassSubjunctive, False) -- Charlie failed the test. If Charlie retook the test tomorrow, Charlie would not pass the test.
        , (failNotFailSubjunctive, True) -- Charlie failed the test. If Charlie retook the test tomorrow, Charlie would not fail the test.
        ]
    , [
        (notPassPassSubjunctive, True) -- Charlie did not pass the test. If Charlie retook the test tomorrow, Charlie would pass the test.
        , (notPassFailSubjunctive, True) -- Charlie did not pass the test. If Charlie retook the test tomorrow, Charlie would fail the test.
        , (notPassNotPassSubjunctive, True) -- Charlie did not pass the test. If Charlie retook the test tomorrow, Charlie would not pass the test.
        , (notPassNotFailSubjunctive, True) -- Charlie did not pass the test. If Charlie retook the test tomorrow, Charlie would not fail the test.
        ]
    , [
        (notFailPassSubjunctive, False) -- Charlie did not fail the test. If Charlie retook the test tomorrow, Charlie would pass the test.
        , (notFailFailSubjunctive, True) -- Charlie did not fail the test. If Charlie retook the test tomorrow, Charlie would fail the test.
        , (notFailNotPassSubjunctive, True) -- Charlie did not fail the test. If Charlie retook the test tomorrow, Charlie would not pass the test.
        , (notFailNotFailSubjunctive, True) -- Charlie did not fail the test. If Charlie retook the test tomorrow, Charlie would not fail the test.
        ]
    ]

counterfactualTimeFocusExpected = [
    [
        (passPassCounterfactual, True)  -- Charlie passed the test. If Charlie had taken the test yesterday, Charlie would have passed the test.
        , (passFailCounterfactual, True) -- Charlie passed the test. If Charlie had taken the test yesterday, Charlie would not have failed the test.
        , (passNotPassCounterfactual, True) -- Charlie passed the test. If Charlie had taken the test yesterday, Charlie would not have failed the test.
        , (passNotFailCounterfactual, False) -- Charlie passed the test. If Charlie had taken the test yesterday, Charlie would not have failed the test.
        ]
    , [
        (failPassCounterfactual, True)  -- Charlie failed the test. If Charlie had taken the test yesterday, Charlie would have passed the test.
        , (failFailCounterfactual, True) -- Charlie failed the test. If Charlie had taken the test yesterday, Charlie would not have failed the test.
        , (failNotPassCounterfactual, False) -- Charlie failed the test. If Charlie had taken the test yesterday, Charlie would not have failed the test.
        , (failNotFailCounterfactual, True) -- Charlie failed the test. If Charlie had taken the test yesterday, Charlie would not have failed the test.
        ]
    , [
        (notPassPassCounterfactual, True)  -- Charlie did not pass the test. If Charlie had taken the test yesterday, Charlie would have passed the test.
        , (notPassFailCounterfactual, False) -- Charlie did not pass the test. If Charlie had taken the test yesterday, Charlie would not have failed the test
        , (notPassNotPassCounterfactual, True) -- Charlie did not pass the test. If Charlie had taken the test yesterday, Charlie would not have failed the test.
        , (notPassNotFailCounterfactual, True) -- Charlie did not pass the test. If Charlie had taken the test yesterday, Charlie would not have failed the test.
        ]
    , [
        (notFailPassCounterfactual, False)  -- Charlie did not fail the test. If Charlie had taken the test yesterday, Charlie would have passed the test.
        , (notFailFailCounterfactual, True) -- Charlie did not fail the test. If Charlie had taken the test yesterday, Charlie would not have failed the test.
        , (notFailNotPassCounterfactual, True) -- Charlie did not fail the test. If Charlie had taken the test yesterday, Charlie would not have failed the test.
        , (notFailNotFailCounterfactual, True) -- Charlie did not fail the test. If Charlie had taken the test yesterday, Charlie would not have failed the test.
        ]
    ]
