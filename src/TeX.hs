{-# LANGUAGE PartialTypeSignatures, LambdaCase #-}
module TeX
  where

import Control.Monad
import Control.Monad.Reader
import Data.Functor

data Mode = Ru | En
    deriving (Show, Eq)

type Resume = Reader Mode String

ru_en :: _ -> _ -> Resume
ru_en r e = ask <&> \case
    Ru -> r
    En -> e

en = ru_en []
ru = flip ru_en $ []


