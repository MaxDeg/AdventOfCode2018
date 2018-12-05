{-# LANGUAGE TypeFamilies #-}

module Day3 where

-- | Imports -------------------------------------------------------------------

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Data.Void                      ( Void )

-- | Types---- -----------------------------------------------------------------

newtype ClaimId = ClaimId Int
  deriving (Show, Eq, Ord)

newtype Inch = Inch Int
  deriving (Show, Eq, Ord)

data Position = Position
  { leftDistance :: Inch
  , topDistance :: Inch }

data ClaimRectangle = ClaimRectangle
  { claimId :: ClaimId
  , position :: Position
  , width :: Inch
  , height :: Inch }

-- | Functions -----------------------------------------------------------------

-- parseClaim :: String -> Parsec Void Char -- ClaimRectangle
parseClaim txt = do
  char '#'
  claimId <- L.decimal
  space1
  char '@'
  space1
  pLeft <- L.decimal
  char ','
  pTop <- L.decimal
  char ':'
  space1
  width <- L.decimal
  char 'x'
  height <- L.decimal

  return $ ClaimRectangle (ClaimId claimId)
                          (Position (Inch pLeft) (Inch pTop))
                          (Inch width)
                          (Inch height)
--  where
  -- claimIdP = do
  --   char '#'
  --   id <- L.decimal
  --   return $ ClaimId id
