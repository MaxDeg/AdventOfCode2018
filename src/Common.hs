module Common
  ( readData
  )
where


-- | Imports -------------------------------------------------------------------

import           Paths_adventofcode2018

import           System.FilePath

-- | Functions -----------------------------------------------------------------

readData :: String -> IO String
readData fileName = do
  filePath <- getDataFileName $ "data" </> fileName
  readFile filePath
