{-# LANGUAGE LambdaCase #-}

module Day1
  ( Frequency
  , calibrate
  , firstFrequencyReachedTwice
  )
where

-- | Imports -------------------------------------------------------------------

import           Common

import           Text.Megaparsec
import           Text.Megaparsec.Char           ( eol )
import qualified Text.Megaparsec.Char.Lexer    as L

import           Data.Void                      ( Void )
import           Data.Set                       ( insert
                                                , member
                                                )

-- | Types ---------------------------------------------------------------------

newtype Frequency = Frequency Int
    deriving (Show, Eq, Ord)

instance Num Frequency where
    (Frequency i0) + (Frequency i1) = Frequency $ i0 + i1
    (Frequency i0) * (Frequency i1) = Frequency $ i0 * i1
    abs (Frequency i) = Frequency $ abs i
    negate (Frequency i) = Frequency $ negate i
    signum (Frequency i) = Frequency $ signum i
    fromInteger i = Frequency $ fromInteger i


-- | Functions -----------------------------------------------------------------

readFrequencyChanges :: IO [Frequency]
readFrequencyChanges = do
  contents <- readData "Day1.txt"
  runParserT frequencyLineP "" contents >>= \case
    Left  err -> fail $ "Error while parsing file" <> show err
    Right fs  -> pure $ fmap Frequency fs
 where
  frequencyP :: ParsecT Void String IO Int
  frequencyP     = L.signed (pure ()) L.decimal
  frequencyLineP = sepEndBy frequencyP eol

applyFrequencyChanges :: Frequency -> [Frequency] -> Frequency
applyFrequencyChanges initialFreq changes = initialFreq + sum changes

calibrate :: IO Frequency
calibrate = applyFrequencyChanges (Frequency 0) <$> readFrequencyChanges

firstFrequencyReachedTwice :: IO Frequency
firstFrequencyReachedTwice =
  filterSingleValue mempty . frequencies . cycle <$> readFrequencyChanges
 where
  frequencies = scanl (+) (Frequency 0)
  filterSingleValue alreadySeen (x : xs) = if member x alreadySeen
    then x
    else filterSingleValue (insert x alreadySeen) xs
