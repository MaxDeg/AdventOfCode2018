{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Day1
import           Day2
import           Day3

main :: IO ()
main = do
  putStrLn "-------------------------------------------------------------------"
  putStrLn "Day 1 - Calibration"
  putStrLn "-------------------------------------------------------------------"
  f <- calibrate
  putStrLn $ "Frequency after calibration: " <> show f
  freqReachedTwice <- firstFrequencyReachedTwice
  putStrLn $ "Frequency reached twice: " <> show freqReachedTwice

  putStrLn "-------------------------------------------------------------------"
  putStrLn "Day 2 - Calibration"
  putStrLn "-------------------------------------------------------------------"
  checksum <- calculateCheckSum
  putStrLn $ "BoxIds checksum: " <> show checksum
  commonLetters <- findCommonLettersInBoxIds
  putStrLn $ "Common Letters: " <> show commonLetters

  putStrLn "-------------------------------------------------------------------"
  putStrLn "Day 3 - Calibration"
  putStrLn "-------------------------------------------------------------------"

  putStrLn "It's done for now"

