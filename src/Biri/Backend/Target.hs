{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Biri.Backend.Target
( scgi_h
, scgi_c
) where

import Data.Text (Text)
import Text.Literal.TH (litFile)

scgi_h :: Text
scgi_h = [litFile|target/scgi.h|]

scgi_c :: Text
scgi_c = [litFile|target/scgi.c|]
