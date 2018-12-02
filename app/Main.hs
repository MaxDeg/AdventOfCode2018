{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Day1
import           Day2

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
  putStrLn $ "BoxId: "

  putStrLn "It's done for now"

