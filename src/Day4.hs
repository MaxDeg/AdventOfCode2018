module Day4 where


-- | Imports -------------------------------------------------------------------

import           Common

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Data.Time


-- | Types ---------------------------------------------------------------------

newtype GuardId = GuardId Int
  deriving (Show, Eq, Ord)

data GuardStatus = FallsAsleep | WakeUp
  deriving (Show, Eq)

data GuardRecord = GuardRecord
  { _guardId :: GuardId
  , _status :: GuardStatus }

type GuardRecordLog = [GuardRecord]

-- | Functions -----------------------------------------------------------------

guardRecordParser :: Parser GuardRecord
guardRecordParser = undefined
 where
  dateTimeP = undefined
  dateP :: Parser (Maybe Day)
  dateP = do
    year <- L.decimal
    char '-'
    month <- L.decimal
    char '-'
    fromGregorianValid year month <$> L.decimal
  shiftStartP :: Parser GuardId
  shiftStartP = do
    string "Guard #"
    gId <- L.decimal
    string " begins shift"
    return $ GuardId gId
  fallsAsleepP :: Parser GuardStatus
  fallsAsleepP = do
    string "falls asleep"
    return FallsAsleep
  wakeUpP :: Parser GuardStatus
  wakeUpP = do
    string "wakes up"
    return WakeUp

