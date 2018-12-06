module Day3Spec
  ( spec
  )
where

import           Day3

import           Test.Hspec

import           Text.Megaparsec

import           Data.Set                       ( singleton )



spec :: Spec
spec = do
  it "#123 @ 3,2: 5x4 should create proper claimrectangle"
    $ let rectangle = runParser claimParser "" "#123 @ 3,2: 5x4"
      in
        rectangle
          `shouldBe` (Right $ ClaimRectangle (ClaimId 123)
                                             (Position 3 2)
                                             (RectangleSize 5 4)
                     )
  it "should parse list of claim"
    $ let rectangles =
            parseClaims ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]
          expectedRectangles =
            [ ClaimRectangle (ClaimId 1) (Position 1 3) (RectangleSize 4 4)
            , ClaimRectangle (ClaimId 2) (Position 3 1) (RectangleSize 4 4)
            , ClaimRectangle (ClaimId 3) (Position 5 5) (RectangleSize 2 2)
            ]
      in  rectangles `shouldBe` expectedRectangles
  it "should calculate proper positions from rectangles"
    $ let positions = calculateSquareInchesFromRectangle
            $ ClaimRectangle (ClaimId 1) (Position 1 3) (RectangleSize 2 2)
      in  positions
            `shouldBe` [Position 2 4, Position 2 5, Position 3 4, Position 3 5]
  it "Overlapping square inches are detected"
    $ let
        rectangles =
          [ ClaimRectangle (ClaimId 1) (Position 1 3) (RectangleSize 4 4)
          , ClaimRectangle (ClaimId 2) (Position 3 1) (RectangleSize 4 4)
          , ClaimRectangle (ClaimId 3) (Position 5 5) (RectangleSize 2 2)
          ]
        positions = fmap calculateSquareInchesFromRectangle rectangles
        overlappingPositions = calculateOverlappingPositions $ concat positions
      in
        do
          positions
            `shouldBe` [ [ Position 2 4
                         , Position 2 5
                         , Position 2 6
                         , Position 2 7
                         , Position 3 4
                         , Position 3 5
                         , Position 3 6
                         , Position 3 7
                         , Position 4 4
                         , Position 4 5
                         , Position 4 6
                         , Position 4 7
                         , Position 5 4
                         , Position 5 5
                         , Position 5 6
                         , Position 5 7
                         ]
                       , [ Position 4 2
                         , Position 4 3
                         , Position 4 4
                         , Position 4 5
                         , Position 5 2
                         , Position 5 3
                         , Position 5 4
                         , Position 5 5
                         , Position 6 2
                         , Position 6 3
                         , Position 6 4
                         , Position 6 5
                         , Position 7 2
                         , Position 7 3
                         , Position 7 4
                         , Position 7 5
                         ]
                       , [ Position 6 6
                         , Position 6 7
                         , Position 7 6
                         , Position 7 7
                         ]
                       ]
          overlappingPositions
            `shouldBe` [Position 4 4, Position 4 5, Position 5 4, Position 5 5]
          countOverlappingSquareInches rectangles `shouldBe` 4
  it "should find Claim 3 has not overlapping"
    $ let rectangles =
            [ ClaimRectangle (ClaimId 1) (Position 1 3) (RectangleSize 4 4)
            , ClaimRectangle (ClaimId 2) (Position 3 1) (RectangleSize 4 4)
            , ClaimRectangle (ClaimId 3) (Position 5 5) (RectangleSize 2 2)
            ]
      in  getNonOverlappingClaim rectangles `shouldBe` [ClaimId 3]

