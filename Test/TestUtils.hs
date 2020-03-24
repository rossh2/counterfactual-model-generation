module Test.TestUtils where

import Utils.TypeClasses

printCorrectCountOrDebugSingle :: (ShowLinear a) => [(a, Bool)] -> IO ()
printCorrectCountOrDebugSingle = printCorrectCountOrDebug showLin

printCorrectCountOrDebugList :: (ShowLinear a) => [([a], Bool)] -> IO ()
printCorrectCountOrDebugList = printCorrectCountOrDebug showLinList

printCorrectCountOrDebug :: (a -> String) -> [(a, Bool)] -> IO ()
printCorrectCountOrDebug showFn tested = do
    let testResults = map snd tested
        correctCount = length $ filter id testResults
        allCount = length testResults
    putStrLn $ "Correctly predicted: " ++ (show correctCount) ++ "/" ++ (show allCount)
    if correctCount /= allCount then debug showFn tested else putStrLn ""

debug :: (a -> String) -> [(a, Bool)] -> IO ()
debug showFn tested = do
    let failed = map (showFn . fst) (filter (not . snd) tested)
    putStrLn $ "Failed: " ++ show failed

countCorrect :: [Bool] -> String
countCorrect isCorrectList = "Correctly predicted: " ++ (show correctCount) ++ "/" ++ (show allCount)
    where correctCount = length $ filter id isCorrectList
          allCount = length isCorrectList