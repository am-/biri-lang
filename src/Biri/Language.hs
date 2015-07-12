
module Biri.Language
( module Biri.Language.AbstractSyntax
, parse
) where

import Data.Text (Text)

import Biri.Language.AbstractSyntax
import Biri.Language.Lexer
import Biri.Language.Parser

parse :: Text -> Either String Program
parse = program . tokenize
