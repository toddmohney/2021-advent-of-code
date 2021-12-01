module Main where

import qualified Safe
import qualified System.Environment as Env

main :: IO ()
main = do
    args <- Env.getArgs

    case Safe.headMay args of
        Nothing ->
            putStrLn "Missing filepath argument"
        (Just inputFilepath) -> do
            input <- map read . lines <$> readFile inputFilepath
            putStrLn $ "Number of depth increases: " <> (show $ countDepthIncreases input)
            putStrLn $ "Number of windowed depth increases: " <> (show $ countDepthIncreasesWindowed input)

countDepthIncreases
    :: [Int]
    -> Int
countDepthIncreases (depth:depths) = countDepthIncreases' 0 depth depths
countDepthIncreases _ = 0


countDepthIncreases'
    :: Int
    -> Int
    -> [Int]
    -> Int
countDepthIncreases' currCount prevDepth [] =
    currCount -- base case

countDepthIncreases' currCount prevDepth (currDepth:depths)
    | currDepth > prevDepth = countDepthIncreases' (currCount + 1) currDepth depths
    | otherwise             = countDepthIncreases' currCount currDepth depths


countDepthIncreasesWindowed
    :: [Int]
    -> Int
countDepthIncreasesWindowed (depth1:depth2:depth3:depths) = countDepthIncreasesWindowed' 0 (sum [depth1, depth2, depth3]) (depth2:depth3:depths)
countDepthIncreasesWindowed _ = 0


countDepthIncreasesWindowed'
    :: Int
    -> Int
    -> [Int]
    -> Int
countDepthIncreasesWindowed' currCount prevDepth (depth1:depth2:depth3:depths)
    | (sum [depth1, depth2, depth3]) > prevDepth = countDepthIncreasesWindowed' (currCount + 1) (sum [depth1, depth2, depth3]) (depth2:depth3:depths)
    | otherwise                                  = countDepthIncreasesWindowed' currCount (sum [depth1, depth2, depth3]) (depth2:depth3:depths)

countDepthIncreasesWindowed' currCount _ _ =
    currCount -- base case


