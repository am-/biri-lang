
module Biri.Language.AbstractSyntax
( Program(..)
, Resource(..)
, Method(..)
, Handler(..)
, Segment(..)
, URI(..)
, Computation(..)
, Signature(..)
, Parameter(..)
, Instruction(..)
, Data(..)
, TypedExpr(..)
, retype
, Expr(..)
, Pattern(..)
, Type(..)
, Literal(..)
, Identifier(..)
, fromIdentifier
, Constructor(..)
, fromConstructor
) where

import Data.Text (Text)

data Program = Program [Resource] [Data] [Computation]
             deriving (Show, Eq, Ord)

data Resource = Resource URI [Handler]
              deriving (Show, Eq, Ord)

data Method = Delete | Get | Post | Put
            deriving (Show, Eq, Ord, Enum)

data Handler = Handler Method [Instruction]
             deriving (Show, Eq, Ord)

data Segment = Fixed Text
             | Pattern Constructor
             deriving (Show, Eq, Ord)

newtype URI = URI [Segment]
            deriving (Show, Eq, Ord)

data Computation = Function Identifier Signature TypedExpr
                 | Component Identifier Signature [Instruction]
                 deriving (Show, Eq, Ord)

data Signature = Signature [Parameter] Type
               deriving (Show, Eq, Ord)

data Parameter = Parameter Identifier Type
               deriving (Show, Eq, Ord)

data Instruction = IfElse TypedExpr [Instruction] [Instruction]
                 | If TypedExpr [Instruction]
                 | Load Identifier Identifier
                 | Store Identifier TypedExpr
                 deriving (Show, Eq, Ord)

data Data = Data Constructor [Identifier] [(Constructor, [Type])]
          deriving (Show, Eq, Ord)

data TypedExpr = TypedExpr Expr (Maybe Type)
               deriving (Show, Eq, Ord)

retype :: TypedExpr -> Type -> TypedExpr
retype (TypedExpr e _) = TypedExpr e . Just

data Expr = Case TypedExpr [(Pattern, TypedExpr)]
          | Application TypedExpr TypedExpr
          | Lambda Identifier TypedExpr
          | DataConstructor Constructor
          | Variable Identifier
          | Match Int
          | Query Identifier
          | Form Identifier
          | Constant Literal
          deriving (Show, Eq, Ord)

data Pattern = ConstructorPattern Constructor [Pattern]
             | VariablePattern Identifier
             | WildcardPattern
             deriving (Show, Eq, Ord)

data Literal = LString Text
             | LInt Int
             | LDouble Double
             deriving (Show, Eq, Ord)

data Type = TypeVariable Identifier
          | TypeConstructor Constructor
          | ComponentType [Parameter] [Parameter]
          | TypeApplication Type Type
          | FunctionArrow Type Type
          deriving (Show, Eq, Ord)

newtype Identifier = Identifier Text
                   deriving (Show, Eq, Ord)

fromIdentifier :: Identifier -> Text
fromIdentifier (Identifier t) = t

newtype Constructor = Constructor Text
                    deriving (Show, Eq, Ord)

fromConstructor :: Constructor -> Text
fromConstructor (Constructor t) = t

