
module Main
( main
) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import qualified Syntax

main :: IO ()
main = flip defaultMainWithOpts mempty
     [ Syntax.tests
     ]
     
