{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Biri.Backend.Rts
( bootstrap_c
, data_h
) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Literal.TH (litFile)

import qualified Biri.Backend.CodeGen.Data as Data
import Biri.Language.AbstractSyntax

data_h :: [Data] -> Either [String] Text
data_h = Data.generate

bootstrap_c :: Text
bootstrap_c = [litFile|rts/bootstrap.c|]


