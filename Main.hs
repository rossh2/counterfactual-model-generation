module Main where

import DataStructures
import ModelGeneration
import Examples

main :: IO ()
main = do
    putStrLn "If Charlie brings his calculator tomorrow, Charlie will pass his test."
    putStrLn $ show $ generateModel indicativeConditional
    putStrLn ""
    putStrLn "If Charlie brought his calculator tomorrow, Charlie would pass his test."
    putStrLn $ show $ generateModel subjunctiveConditional
    putStrLn ""
    putStrLn "If Charlie had brought his calculator yesterday, Charlie would have passed his test."
    putStrLn $ show $ generateModel pastCounterfactual
