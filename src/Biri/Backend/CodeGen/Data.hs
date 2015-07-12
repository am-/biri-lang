{-# LANGUAGE OverloadedStrings #-}

module Biri.Backend.CodeGen.Data
( generate
, renameType
, renameConstructor
) where

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Data.Either (partitionEithers)
import Data.Text (Text)
import qualified Data.Text as T

import Biri.Language.AbstractSyntax

type Declaration = (Text, [Text], [(Text, [Type])])

generate :: [Data] -> Either [String] Text
generate tokens = case partitionEithers (map (check . prepare) tokens) of
    ([], decls) -> Right . enclose . T.concat $ map compile decls
    (errors, _) -> Left (concat errors)
  where
    enclose :: Text -> Text
    enclose = T.append "#ifndef _DATA_H\n#define _DATA_H\n\n"
            . flip T.append "\n#endif"

compile :: Declaration -> Text
compile (name, types, constrs) = T.unlines
    [ T.unwords ["struct", renameType name, "{"]
    , "  int tag;"
    , case catMaybes (map (fmap (T.append "    ") . uncurry constructor2text) constrs) of
          [] -> "  // Nothing to do"
          structs -> T.intercalate "\n" ["  union {" , T.intercalate "\n"  structs, "  };"]
    , "};"
    ]
  where
    constructor2text :: Text -> [Type] -> Maybe Text
    constructor2text constr types
        | null types = Nothing
        | otherwise  = Just $ T.unwords
            [ "struct", "{"
            , T.unwords (zipWith (\i t -> T.unwords [type2text t, T.snoc (label constr i) ';']) [0..] types)
            , "};"
            ]
    
    label :: Text -> Int -> Text
    label constr = T.append constr . T.pack . show
    
    type2text :: Type -> Text
    type2text typ = case typ of
        TypeConstructor (Constructor constr) _ -> T.append "struct " (T.snoc (renameType constr) '*')
        FunctionArrow _ _ -> "void*"
        TypeVariable _ -> "void*"
        ComponentType _ _ -> "void*"

renameType :: Text -> Text
renameType = T.append "wasm_type_"

renameConstructor :: Text -> Text
renameConstructor = T.append "wasm_constr_"

check :: Declaration -> Either [String] Declaration
check decl@(name, vars, constrs) = case concatMap (uncurry checkConstructor) constrs of
    []     -> Right decl
    errors -> Left errors
  where
    checkConstructor :: Text -> [Type] -> [String]
    checkConstructor constr = concatMap (checkType constr)
    
    checkType :: Text -> Type -> [String]
    checkType constr typ = case typ of
        TypeConstructor _ types -> concatMap (checkType constr) types
        FunctionArrow typ1 typ2 -> concatMap (checkType constr) [typ1, typ2]
        TypeVariable (Identifier v) | v `elem` vars -> []
                                    | otherwise     -> [unknownVariable constr v]
        ComponentType ins outs -> concatMap (\(Parameter _ t) -> checkType constr t) (ins ++ outs)
    
    unknownVariable :: Text -> Text -> String
    unknownVariable constr v = unwords
        [ "Unknown type variable", T.unpack v
        , "in constructor", T.unpack constr
        , "of declaration ", T.unpack name
        ]

prepare :: Data -> Declaration
prepare (Data (Constructor name) vars constrs) =
    (,,) name (map fromIdentifier vars) (map fromTypeConstructor constrs)
  where
    fromTypeConstructor :: Type -> (Text, [Type])
    fromTypeConstructor token = case token of
        TypeConstructor (Constructor constr) types -> (constr, types)
        _ -> error "prepare.fromTypeConstructor: The parser guarantees type constructors at this point."
