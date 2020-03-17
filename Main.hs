module Main where

import DataStructures
import ModelGeneration
import Examples

main :: IO ()
main = do
    standardConditionals
    putStrLn ""
    timeContrastConditionals
    putStrLn ""
    discourses

standardConditionals :: IO ()
standardConditionals = do
    standardPassConditionals
    putStrLn ""
    standardNotFailConditionals

standardPassConditionals :: IO ()
standardPassConditionals = do
    putStrLn "STANDARD CONDITIONALS - PASS"
    putStrLn "If Charlie brings his calculator tomorrow, Charlie will pass his test."
    putStrLn $ show $ generateModel indicativePassConditional
    putStrLn ""
    putStrLn "If Charlie brought his calculator tomorrow, Charlie would pass his test."
    putStrLn $ show $ generateModel subjunctivePassConditional
    putStrLn ""
    putStrLn "If Charlie had brought his calculator yesterday, Charlie would have passed his test."
    putStrLn $ show $ generateModel pastPassCounterfactual
    putStrLn ""
    putStrLn "If Charlie takes his test tomorrow, Charlie will pass his test."
    putStrLn $ show $ generateModel indicativeTimeNoFocusPassConditional
    putStrLn ""

standardNotFailConditionals :: IO ()
standardNotFailConditionals = do
    putStrLn "STANDARD CONDITIONALS - NOT FAIL"
    putStrLn "If Charlie brings his calculator tomorrow, Charlie won't fail his test."
    putStrLn $ show $ generateModel indicativeNotFailConditional
    putStrLn ""
    putStrLn "If Charlie brought his calculator tomorrow, Charlie wouldn't fail his test."
    putStrLn $ show $ generateModel subjunctiveNotFailConditional
    putStrLn ""
    putStrLn "If Charlie had brought his calculator yesterday, Charlie wouldn't have failed his test."
    putStrLn $ show $ generateModel pastNotFailCounterfactual
    putStrLn ""
    putStrLn "If Charlie takes his test tomorrow, Charlie won't fail his test."
    putStrLn $ show $ generateModel indicativeTimeNoFocusNotFailConditional
    putStrLn ""

timeContrastConditionals :: IO ()
timeContrastConditionals = do
    timeContrastPassConditionals
    putStrLn ""
    timeContrastNotFailConditionals

timeContrastPassConditionals :: IO ()
timeContrastPassConditionals = do
    putStrLn "CONDITIONALS WITH FOCUSED TIME / EVENT REPETITION - PASS"
    putStrLn "If Charlie re-takes his test tomorrow, Charlie will pass his test."
    putStrLn $ show $ generateModel indicativeTimeFocusPassConditional
    putStrLn ""
    putStrLn "If Charlie re-took his test tomorrow, Charlie would pass his test."
    putStrLn $ show $ generateModel subjunctiveTimeFocusPassConditional
    putStrLn ""
    putStrLn "If Charlie had taken his test yesterday, Charlie would have passed his test."
    putStrLn $ show $ generateModel pastTimeFocusPassCounterfactual
    putStrLn ""
    putStrLn "If Charlie had taken his test tomorrow, Charlie would have passed his test."
    putStrLn $ show $ generateModel futureTimeFocusPassCounterfactual

timeContrastNotFailConditionals :: IO ()
timeContrastNotFailConditionals = do
    putStrLn "CONDITIONALS WITH FOCUSED TIME / EVENT REPETITION - NOT FAIL"
    putStrLn "If Charlie re-takes his test tomorrow, Charlie won't fail his test."
    putStrLn $ show $ generateModel indicativeTimeFocusNotFailConditional
    putStrLn ""
    putStrLn "If Charlie re-took his test tomorrow, Charlie wouldn't fail his test."
    putStrLn $ show $ generateModel subjunctiveTimeFocusNotFailConditional
    putStrLn ""
    putStrLn "If Charlie had taken his test yesterday, Charlie wouldn't have failed his test."
    putStrLn $ show $ generateModel pastTimeFocusNotFailCounterfactual
    putStrLn ""
    putStrLn "If Charlie had taken his test tomorrow, Charlie wouldn't have failed his test."
    putStrLn $ show $ generateModel futureTimeFocusNotFailCounterfactual

discourses :: IO ()
discourses = do
    failNotFailDiscourses
    putStrLn ""
    notFailNotFailDiscourses


failNotFailDiscourses :: IO ()
failNotFailDiscourses = do
    putStrLn "TWO-SENTENCE DISCOURSES - FAIL vs. NOT FAIL"
    let sentenceModel = generateModel failPastSentence
    putStrLn "Charlie failed his test. If Charlie re-takes his test tomorrow, Charlie won't fail his test."
    let conditionalModel = generateModel indicativeTimeFocusNotFailConditional
        combinedModel = combineModels (Just sentenceModel) (Just conditionalModel)
    putStrLn $ show $ combinedModel
    putStrLn ""
    putStrLn "Charlie failed his test. If Charlie re-took his test tomorrow, Charlie wouldn't fail his test."
    let conditionalModel = generateModel subjunctiveTimeFocusNotFailConditional
        combinedModel = combineModels (Just sentenceModel) (Just conditionalModel)
    putStrLn $ show $ combinedModel
    putStrLn ""
    putStrLn "Charlie failed his test. If Charlie had taken his test yesterday, Charlie wouldn't have failed his test."
    let conditionalModel = generateModel pastTimeFocusNotFailCounterfactual
        combinedModel = combineModels (Just sentenceModel) (Just conditionalModel)
    putStrLn $ show $ combinedModel
    putStrLn ""
    putStrLn "Charlie failed his test. If Charlie had taken his test tomorrow, Charlie wouldn't have failed his test."
    let conditionalModel = generateModel futureTimeFocusNotFailCounterfactual
        combinedModel = combineModels (Just sentenceModel) (Just conditionalModel)
    putStrLn $ show $ combinedModel
    putStrLn ""

notFailNotFailDiscourses :: IO ()
notFailNotFailDiscourses = do
    putStrLn "TWO-SENTENCE DISCOURSES - NOT FAIL vs. NOT FAIL"
    let sentenceModel = generateModel notFailPastSentence
    putStrLn "Charlie didn't fail his test. If Charlie re-takes his test tomorrow, Charlie won't fail his test."
    let conditionalModel = generateModel indicativeTimeFocusNotFailConditional
        combinedModel = combineModels (Just sentenceModel) (Just conditionalModel)
    putStrLn $ show $ combinedModel
    putStrLn ""
    putStrLn "Charlie didn't fail his test. If Charlie re-took his test tomorrow, Charlie wouldn't fail his test."
    let conditionalModel = generateModel subjunctiveTimeFocusNotFailConditional
        combinedModel = combineModels (Just sentenceModel) (Just conditionalModel)
    putStrLn $ show $ combinedModel
    putStrLn ""
    putStrLn "Charlie didn't fail his test. If Charlie had taken his test yesterday, Charlie wouldn't have failed his test."
    let conditionalModel = generateModel pastTimeFocusNotFailCounterfactual
        combinedModel = combineModels (Just sentenceModel) (Just conditionalModel)
    putStrLn $ show $ combinedModel
    putStrLn ""
    putStrLn "Charlie didn't fail his test. If Charlie had taken his test tomorrow, Charlie wouldn't have failed his test."
    let conditionalModel = generateModel futureTimeFocusNotFailCounterfactual
        combinedModel = combineModels (Just sentenceModel) (Just conditionalModel)
    putStrLn $ show $ combinedModel
    putStrLn ""