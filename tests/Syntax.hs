{-# LANGUAGE OverloadedStrings #-}

module Syntax
( tests
) where

import Control.Applicative ((<$>))
import Control.Monad (zipWithM_)
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Biri.Language

tests = [ testCase "Bool" bool
        , testCase "Bool-annotated" boolAnnotated
        ]

tycon :: Text -> Type
tycon = TypeConstructor . Constructor

tyvar :: Text -> Type
tyvar = TypeVariable . Identifier

dacon :: Text -> [Type] -> (Constructor, [Type])
dacon = (,) . Constructor

var :: Text -> Expr
var = Variable . Identifier

bool = do
    output <- parse <$> T.readFile "tests/syntax/Bool.biri"
    case output of
        Left msg -> assertFailure msg
        Right (Program resources datas functions) -> do
            assertEqual "Data declaration `Bool'" [data_bool] datas
            let fns = zip ["not", "convolute", "and", "or", "bool"] [fn_not, fn_convolute, fn_and, fn_or, fn_bool]
            zipWithM_ (\(label, fn) -> assertEqual ("Definition of `" ++ label ++ "'") fn) fns functions
  where
    data_bool = Data (Constructor "Bool") []
              [ dacon "False" []
              , dacon "True" []
              ]
    
    fn_not = Function
        (Identifier "not")
        (Signature [Parameter (Identifier "b") (tycon "Bool")] (tycon "Bool"))
        (flip TypedExpr Nothing (Case (TypedExpr (var "b") Nothing)
            [ (ConstructorPattern (Constructor "False") [], TypedExpr (DataConstructor (Constructor "True")) Nothing)
            , (ConstructorPattern (Constructor "True") [], TypedExpr (DataConstructor (Constructor "False")) Nothing)
            ]
            )
        )
    
    fn_convolute = Function
        (Identifier "convolute")
        (Signature [Parameter (Identifier "b") (tycon "Bool")] (tycon "Bool"))
        (TypedExpr
            (Application
                (TypedExpr (var "not") Nothing)
                (TypedExpr
                    (Application
                        (TypedExpr (var "not") Nothing)
                        (TypedExpr (var "b") Nothing))
                    Nothing))
            Nothing)
    
    fn_and = Function
        (Identifier "and")
        (Signature [Parameter (Identifier "b") (tycon "Bool")] (FunctionArrow (tycon "Bool") (tycon "Bool")))
        (TypedExpr
            (Case
                (TypedExpr (var "b") Nothing)
                [ (ConstructorPattern (Constructor "False") [], TypedExpr (Lambda (Identifier "x") (TypedExpr (DataConstructor (Constructor "False")) Nothing)) Nothing)
                , (ConstructorPattern (Constructor "True") [], TypedExpr (Lambda (Identifier "x") (TypedExpr (var "x") Nothing)) Nothing)
                ]
            )
            Nothing
        )

    fn_or = Function
        (Identifier "or")
        (Signature [Parameter (Identifier "b") (tycon "Bool")] (FunctionArrow (tycon "Bool") (tycon "Bool")))
        (TypedExpr
            (Lambda
                (Identifier "x")
                (TypedExpr
                    (Case
                        (TypedExpr (var "b") Nothing)
                        [ (ConstructorPattern (Constructor "False") [], TypedExpr (var "x") Nothing)
                        , (ConstructorPattern (Constructor "True") [], TypedExpr (DataConstructor (Constructor "True")) Nothing)
                        ]       
                    )
                    Nothing
                )
            )
            Nothing
        )

    fn_bool = Function
        (Identifier "bool")
        (Signature [Parameter (Identifier "x") (tyvar "a"), Parameter (Identifier "y") (tyvar "a"), Parameter (Identifier "b") (tycon "Bool")] (tycon "Bool"))
        (TypedExpr
            (Case
                (TypedExpr (var "b") Nothing)
                [ (ConstructorPattern (Constructor "False") [], TypedExpr (var "y") Nothing)
                , (ConstructorPattern (Constructor "True") [], TypedExpr (var "x") Nothing)
                ]
            )
            Nothing
        )

boolAnnotated = do
    output <- parse <$> T.readFile "tests/syntax/Bool-annotated.biri"
    case output of
        Left msg -> assertFailure msg
        Right (Program resources datas functions) -> do
            assertEqual "Data declaration `Bool'" [data_bool] datas
            let fns = zip ["not", "convolute", "and", "or", "bool"] [fn_not, fn_convolute, fn_and, fn_or, fn_bool]
            zipWithM_ (\(label, fn) -> assertEqual ("Definition of `" ++ label ++ "'") fn) fns functions
  where
    data_bool = Data (Constructor "Bool") []
              [ dacon "False" []
              , dacon "True" []
              ]
    
    fn_not = Function
        (Identifier "not")
        (Signature [Parameter (Identifier "b") (tycon "Bool")] (tycon "Bool"))
        (flip TypedExpr (Just $ tycon "Bool") (Case (TypedExpr (var "b") (Just $ tycon "Bool"))
            [ (ConstructorPattern (Constructor "False") [], TypedExpr (DataConstructor (Constructor "True")) (Just $ tycon "Bool"))
            , (ConstructorPattern (Constructor "True") [], TypedExpr (DataConstructor (Constructor "False")) (Just $ tycon "Bool"))
            ]
            )
        )
    
    fn_convolute = Function
        (Identifier "convolute")
        (Signature [Parameter (Identifier "b") (tycon "Bool")] (tycon "Bool"))
        (TypedExpr
            (Application
                (TypedExpr (var "not") Nothing)
                (TypedExpr
                    (Application
                        (TypedExpr (var "not") (Just $ FunctionArrow (tycon "Bool") (tycon "Bool")))
                        (TypedExpr (var "b") (Just $ tycon "Bool")))
                    (Just $ tycon "Bool")))
            (Just $ tycon "Bool"))
    
    fn_and = Function
        (Identifier "and")
        (Signature [Parameter (Identifier "b") (tycon "Bool")] (FunctionArrow (tycon "Bool") (tycon "Bool")))
        (TypedExpr
            (Case
                (TypedExpr (var "b") Nothing)
                [ (ConstructorPattern (Constructor "False") [], TypedExpr (Lambda (Identifier "x") (TypedExpr (DataConstructor (Constructor "False")) (Just $ tycon "Bool"))) Nothing)
                , (ConstructorPattern (Constructor "True") [], TypedExpr (Lambda (Identifier "x") (TypedExpr (var "x") Nothing)) (Just $ FunctionArrow (tycon "Bool") (tycon "Bool")))
                ]
            )
            Nothing
        )

    fn_or = Function
        (Identifier "or")
        (Signature [Parameter (Identifier "b") (tycon "Bool")] (FunctionArrow (tycon "Bool") (tycon "Bool")))
        (TypedExpr
            (Lambda
                (Identifier "x")
                (TypedExpr
                    (Case
                        (TypedExpr (var "b") Nothing)
                        [ (ConstructorPattern (Constructor "False") [], TypedExpr (var "x") (Just $ tycon "Bool"))
                        , (ConstructorPattern (Constructor "True") [], TypedExpr (DataConstructor (Constructor "True")) (Just $ tycon "Bool"))
                        ]       
                    )
                    Nothing
                )
            )
            (Just $ FunctionArrow (tycon "Bool") (tycon "Bool"))
        )

    fn_bool = Function
        (Identifier "bool")
        (Signature [Parameter (Identifier "x") (tyvar "a"), Parameter (Identifier "y") (tyvar "a"), Parameter (Identifier "b") (tycon "Bool")] (tycon "Bool"))
        (TypedExpr
            (Case
                (TypedExpr (var "b") (Just $ tycon "Bool"))
                [ (ConstructorPattern (Constructor "False") [], TypedExpr (var "y") (Just $ tyvar "a"))
                , (ConstructorPattern (Constructor "True") [], TypedExpr (var "x") (Just $ tyvar "a"))
                ]
            )
            (Just $ tycon "Bool")
        )
