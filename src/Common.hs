module Common
  ( Parser
  , readData
  )
where


-- | Imports -------------------------------------------------------------------

import           Paths_adventofcode2018

import           System.FilePath

import           Text.Megaparsec

import           Data.Void                      ( Void )


-- | Types ---------------------------------------------------------------------

type Parser = Parsec Void String

-- | Functions -----------------------------------------------------------------

readData :: String -> IO String
readData fileName = do
  filePath <- getDataFileName $ "data" </> fileName
  readFile filePath
