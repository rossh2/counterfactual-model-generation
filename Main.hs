module Main where

import DataStructures
import ModelGeneration
import Examples

main :: IO ()
main = do
    putStrLn "STANDARD CONDITIONALS"
    putStrLn "If Charlie brings his calculator tomorrow, Charlie will pass his test."
    putStrLn $ show $ generateModel indicativeConditional
    putStrLn ""
    putStrLn "If Charlie brought his calculator tomorrow, Charlie would pass his test."
    putStrLn $ show $ generateModel subjunctiveConditional
    putStrLn ""
    putStrLn "If Charlie had brought his calculator yesterday, Charlie would have passed his test."
    putStrLn $ show $ generateModel pastCounterfactual
    putStrLn ""
    putStrLn "If Charlie takes his test tomorrow, Charlie will pass his test."
    putStrLn $ show $ generateModel indicativeTimeNoFocusConditional
    putStrLn ""
    putStrLn "\nCONDITIONALS WITH FOCUSED TIME / EVENT REPETITION"
    putStrLn "If Charlie re-takes his test tomorrow, Charlie will pass his test."
    putStrLn $ show $ generateModel indicativeTimeFocusConditional
    putStrLn ""
    putStrLn "If Charlie re-took his test tomorrow, Charlie would pass his test."
    putStrLn $ show $ generateModel subjunctiveTimeFocusConditional
    putStrLn ""
    putStrLn "If Charlie had taken his test yesterday, Charlie would have passed his test."
    putStrLn $ show $ generateModel pastTimeFocusCounterfactual
    putStrLn ""
    putStrLn "If Charlie had taken his test tomorrow, Charlie would have passed his test."
    putStrLn $ show $ generateModel futureTimeFocusCounterfactual

