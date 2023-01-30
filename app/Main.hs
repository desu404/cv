{-# LANGUAGE LambdaCase #-}
module Main (main)
  where

import System.Environment

main :: IO ()
main = getArgs >>= \case
    ["ru"] -> pure ()
    ["en"] -> pure ()
    _ -> putStrLn $ unlines
        [ "Usage: resume LANG"
        , "LANG is ru or en."
        ]
