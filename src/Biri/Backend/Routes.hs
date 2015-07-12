{-# LANGUAGE OverloadedStrings #-}

module Biri.Backend.Routes
( generate
) where

import Control.Monad (mfilter)
import Data.Char (toLower, toUpper)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Biri.Backend.Routes.Automaton
import Biri.Language.AbstractSyntax

generate :: [Resource] -> Text
generate resources = T.unlines
    [ "#include <stdio.h>"
    , "#include <string.h>"
    , ""
    , T.concat ["int next_state[", T.pack (show (1 + states automaton)), "][128] = {"]
    , T.concat ["  ", T.intercalate ",\n  " (map genTable $ V.toList (delta automaton))]
    , "};"
    , ""
    , T.intercalate "\n" (map genHandler [Delete .. Put])
    , ""
    , "const char *match(const char *method, const char *uri) {"
    , "  int state = 0;"
    , "  for(int i = 0, length = strlen(uri); i < length; i++) state = next_state[state][uri[i]];"
    , T.intercalate "\n" (map genReturn [Delete .. Put])
    , "  return NULL;"
    , "}"
    ]
  where
    automaton :: Automaton
    automaton = transform resources
    
    genTable :: Map Char [Int] -> Text
    genTable = (T.append "{") . (flip T.append "}") . T.intercalate ", " . map (T.pack . show . head)
             . flip map (take 128 ['\0'..]) . flip (M.findWithDefault [states automaton])
    
    genHandler :: Method -> Text
    genHandler method = T.concat
        [ "char *handler_", T.toLower . T.pack $ show method, "[", T.pack $ show (1 + states automaton), "] = { "
        , T.intercalate ", " (map mkHandler [0..states automaton])
        , "};"
        ]
      where
        mkHandler :: Int -> Text
        mkHandler = fromMaybe "NULL" . fmap (T.pack . show . show) 
                  . mfilter (not . null) . fmap (filter (\(Handler m _) -> method == m))
                  . flip IM.lookup (handlers automaton)
    
    genReturn :: Method -> Text
    genReturn method = T.concat
        [ "  if(strcmp(method, \"", T.toUpper . T.pack $ show method, "\") == 0)"
        , " return handler_",  T.toLower . T.pack $ show method, "[state];"
        ]

