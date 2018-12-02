{-# LANGUAGE OverloadedStrings #-}

module Common
  ( readData
  )
where


-- | Imports -------------------------------------------------------------------

import           Paths_adventofcode2018

import           System.FilePath

import           Data.Text                      ( Text
                                                , unpack
                                                )

-- | Functions -----------------------------------------------------------------

readData :: Text -> IO String
readData fileName = do
  filePath <- getDataFileName $ "data" </> unpack fileName
  readFile filePath
