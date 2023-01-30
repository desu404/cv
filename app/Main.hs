{-# LANGUAGE LambdaCase #-}
module Main (main)
  where

import Control.Monad.Reader
import System.Environment
import TeX

main :: IO ()
main = getArgs >>= \case
    ["ru"] -> putStrLn $ runReader resume Ru
    ["en"] -> putStrLn $ runReader resume En
    _ -> putStrLn $ unlines
        [ "Usage: resume LANG"
        , "LANG is ru or en."
        ]
