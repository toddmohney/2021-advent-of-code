module DayTwo.Lib
    ( Config(..)
    , parseInput
    , getTotalMovement
    ) where

import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T

data Direction
    = Up
    | Down
    | Forward
    deriving (Show, Eq)

instance Read Direction where
    readsPrec _ = \s -> case s of
        "up" -> [(Up, "")]
        "down" -> [(Down, "")]
        "forward" -> [(Forward, "")]
        _ -> []

data Movement = Movement
    { direction :: Direction
    , distance :: Int
    } deriving (Show)

data MovementCount = MovementCount
    { horizontalTotal :: Int
    , verticalTotal :: Int
    , aimTotal :: Int
    } deriving (Show)

data Config
    = WithAimCalculation
    | WithoutAimCalculation

parseInput
    :: [Text]
    -> [Movement]
parseInput = fmap inputToMovement


inputToMovement
    :: Text
    -> Movement
inputToMovement input =
    Movement
        { direction = read . T.unpack $ L.head parts -- partial function errors not handled
        , distance = read . T.unpack $ L.last parts -- partial function errors not handled
        }
  where
    parts = T.words input


getTotalMovement
    :: Config
    -> [Movement]
    -> Int
getTotalMovement config movements =
    horizontalTotal totalMovement * verticalTotal totalMovement
  where
    totalMovement =
        L.foldl' (foldFn config) defaultMovementCount movements

    defaultMovementCount = MovementCount
        { horizontalTotal = 0
        , verticalTotal = 0
        , aimTotal = 0
        }


foldFn
    :: Config
    -> MovementCount
    -> Movement
    -> MovementCount
foldFn config acc movement =
    case direction movement of
        Up -> case config of
            WithAimCalculation ->
                acc { aimTotal = aimTotal acc - distance movement }
            WithoutAimCalculation ->
                acc { verticalTotal = verticalTotal acc - distance movement }

        Down -> case config of
            WithAimCalculation ->
                acc { aimTotal = aimTotal acc + distance movement }
            WithoutAimCalculation ->
                acc { verticalTotal = verticalTotal acc + distance movement }

        Forward -> case config of
            WithAimCalculation ->
                acc
                    { horizontalTotal = horizontalTotal acc + distance movement
                    , verticalTotal = verticalTotal acc + (distance movement * aimTotal acc)
                    }
            WithoutAimCalculation ->
                acc { horizontalTotal = horizontalTotal acc + distance movement }
