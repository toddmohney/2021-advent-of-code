module Main where

import qualified Data.Text as T
import qualified Safe
import qualified System.Environment as Env

import qualified DayOne.Lib as Day1
import qualified DayTwo.Lib as Day2

main :: IO ()
main = do
    args <- Env.getArgs

    case Safe.headMay args of
        Nothing ->
            putStrLn "Missing filepath argument"
        (Just inputDir) -> do
            input1 <- map read . lines <$> readFile (inputDir <> "/day1/input")
            putStrLn $ "Day 1:"
            putStrLn $ "Number of depth increases: " <> (show $ Day1.countDepthIncreases input1)
            putStrLn $ "Number of windowed depth increases: " <> (show $ Day1.countDepthIncreasesWindowed input1)
            putStrLn $ "--------------------------------------------"

            input2 <- map T.pack . lines <$> readFile (inputDir <> "/day2/input")
            putStrLn $ "Day 2:"
            putStrLn $ "Total movement: " <> (show . Day2.getTotalMovement Day2.WithoutAimCalculation$ Day2.parseInput input2)
            putStrLn $ "Total movement with aim: " <> (show . Day2.getTotalMovement Day2.WithAimCalculation$ Day2.parseInput input2)
            putStrLn $ "--------------------------------------------"
