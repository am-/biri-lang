
module Main
( main
) where

import Data.Monoid (mempty)
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import qualified Syntax

main :: IO ()
main = flip defaultMainWithOpts mempty
     [ testGroup "Syntax" Syntax.tests
     ]
     
