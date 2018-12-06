{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day3
  ( ClaimId(..)
  , Inch(..)
  , Position(..)
  , ClaimRectangle(..)
  , RectangleSize(..)
  , claimParser
  , parseClaims
  , calculateSquareInchesFromRectangle
  , calculateOverlappingPositions
  , countOverlappingSquareInches
  , findOverlappingSquareInches
  , getNonOverlappingClaim
  , findNotOverlappingClaim
  )
where

-- | Imports -------------------------------------------------------------------

import           Common

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Data.Void                      ( Void )
import           Data.Either                    ( rights )
import           Data.List                      ( group
                                                , sort
                                                , intersect
                                                , any
                                                )
import           Data.Set                       ( Set
                                                , insert
                                                , member
                                                , elemAt
                                                , size
                                                )


-- | Types---- -----------------------------------------------------------------

newtype ClaimId = ClaimId Int
  deriving (Show, Eq, Ord)

newtype Inch = Inch Int
  deriving (Show, Eq, Ord, Enum, Num)

data Position = Position
  { leftDistance :: Inch
  , topDistance :: Inch }
  deriving (Show, Eq, Ord)

data RectangleSize = RectangleSize
  { width :: Inch
  , height :: Inch }
  deriving (Show, Eq)

data ClaimRectangle = ClaimRectangle
  { _claimId :: ClaimId
  , _position :: Position
  , _size :: RectangleSize }
  deriving (Show, Eq)

type Parser = Parsec Void String

-- | Functions -----------------------------------------------------------------

claimParser :: Parser ClaimRectangle
claimParser = do
  claimId <- claimIdP
  space1
  char '@'
  space1
  position <- positionP
  char ':'
  space1
  size <- sizeP

  return $ ClaimRectangle claimId position size
 where
  claimIdP :: Parser ClaimId
  claimIdP = do
    char '#'
    ClaimId <$> L.decimal
  positionP :: Parser Position
  positionP = do
    pLeft <- L.decimal
    char ','
    pTop <- L.decimal
    return $ Position (Inch pLeft) (Inch pTop)
  sizeP :: Parser RectangleSize
  sizeP = do
    width <- L.decimal
    char 'x'
    height <- L.decimal
    return $ RectangleSize (Inch width) (Inch height)

readClaimsData :: IO [String]
readClaimsData = lines <$> readData "Day3.txt"

parseClaims :: [String] -> [ClaimRectangle]
parseClaims claimsData = rights claims
  where claims = fmap (runParser claimParser "Claims Data") claimsData

calculateSquareInchesFromRectangle :: ClaimRectangle -> [Position]
calculateSquareInchesFromRectangle ClaimRectangle { _position = p, _size = s }
  = [ Position (x + left) (y + top)
    | x <- [1 .. (width s)]
    , y <- [1 .. (height s)]
    ]
 where
  left = leftDistance p
  top  = topDistance p

calculateOverlappingPositions :: [Position] -> [Position]
calculateOverlappingPositions =
  fmap head . filter (\ps -> length ps > 1) . group . sort

countOverlappingSquareInches :: [ClaimRectangle] -> Int
countOverlappingSquareInches =
  length
    . calculateOverlappingPositions
    . concatMap calculateSquareInchesFromRectangle

findOverlappingSquareInches :: IO Int
findOverlappingSquareInches =
  countOverlappingSquareInches . parseClaims <$> readClaimsData

getOverlappingPositions :: ClaimRectangle -> ClaimRectangle -> [Position]
getOverlappingPositions r1 r2 = calculateSquareInchesFromRectangle r1
  `intersect` calculateSquareInchesFromRectangle r2

areOverlapping :: ClaimRectangle -> ClaimRectangle -> Bool
areOverlapping ClaimRectangle { _position = p1, _size = s1 } ClaimRectangle { _position = p2, _size = s2 }
  = not
    $  leftDistance p2
    >= leftDistance p1
    +  width s1
    || leftDistance p2
    +  width s2
    <= leftDistance p1
    || topDistance p2
    >= topDistance p1
    +  height s1
    || topDistance p2
    +  height s2
    <= topDistance p1

getNonOverlappingClaim :: [ClaimRectangle] -> [ClaimId]
getNonOverlappingClaim claims =
  [ _claimId r1
  | r1 <- claims
  , not (any (\i -> _claimId r1 /= _claimId i && areOverlapping r1 i) claims)
  ]

findNotOverlappingClaim :: IO ClaimId
findNotOverlappingClaim = do
  claimIds <- getNonOverlappingClaim . parseClaims <$> readClaimsData
  if length claimIds > 1
    then fail "More than one claim found"
    else pure $ head claimIds
