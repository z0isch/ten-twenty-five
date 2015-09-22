{-# LANGUAGE DeriveGeneric #-}

module Game where

import GHC.Generics (Generic)
import Data.Time
import qualified Data.List as L
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C

type BasketResult = Bool
type Distance = Int
data Round = Round [BasketResult] Distance
    deriving (Show,Generic)
instance ToJSON Round
instance FromJSON Round
type Game = [Round]
data GameSave = GameSave
  { gameSaveTime :: UTCTime
  , gameSaveGame :: Game
  }
  deriving (Show,Generic)
instance ToJSON GameSave
instance FromJSON GameSave
data RoundPercent = RoundPercent Double Distance
    deriving (Show)
data RoundMake = RoundMake
  { roundMakeMakes :: Int
  , roundMakeAttempts :: Int
  , roundMakeDistance :: Distance
  }
  deriving (Show)

data ScoreChart = ScoreChart [String] [Int]
  deriving (Show)
instance ToJSON ScoreChart where
  toJSON (ScoreChart ts ss) = object [T.pack "labels" .= ts, T.pack "series" .= [ss]]
 
getScoreChart :: [GameSave] -> ScoreChart
getScoreChart gs = ScoreChart (map (show.gameSaveTime) gs) (map (scoreGame.gameSaveGame) gs)

avg rs = realToFrac (sum rs) / L.genericLength rs

getRoundMake :: Round -> RoundMake
getRoundMake r@(Round br d) = RoundMake (getMadeBaskets r) (length br) d

getRoundMakes :: GameSave -> [RoundMake]
getRoundMakes (GameSave _ rs) = map getRoundMake rs

getAvgRoundPercents :: [GameSave] -> [RoundPercent]
getAvgRoundPercents gs = map avgRPercents $ L.transpose rMakes
  where rMakes = map getRoundMakes gs
        avgRPercents :: [RoundMake] -> RoundPercent
        avgRPercents rms = RoundPercent (100 * fromIntegral (sum $ map roundMakeMakes rms) / fromIntegral (sum $ map roundMakeAttempts rms)) $ roundMakeDistance $ head rms

bestScore :: [GameSave] -> Int
bestScore [] = 0
bestScore gs = maximum $ map (\(GameSave _ g) -> scoreGame g) gs

averageScore :: [GameSave] -> Double
averageScore gs = avg scores
  where scores = map (\(GameSave _ g) -> scoreGame g) gs

scoreGame :: Game -> Int
scoreGame = sum . map scoreRound

scoreRound :: Round -> Int
scoreRound r = madeShotScore r + madeAllScore r + madeFirstLastScore r

madeFirstLastScore :: Round -> Int
madeFirstLastScore (Round bs@(b:_) d) = (multiplier * boolToInt b) + (multiplier * boolToInt (last bs))
  where
    multiplier
      | d < 30    = 5
      | otherwise = 10
madeFirstLastScore _ = 0

madeAllScore :: Round -> Int
madeAllScore (Round bs d) = d * boolToInt (and bs)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt _ = 0

madeShotScore :: Round -> Int
madeShotScore r@(Round bs d) = d * getMadeBaskets r

getMadeBaskets :: Round -> Int
getMadeBaskets (Round bs _) = sum $ map boolToInt bs
