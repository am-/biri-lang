{
module Biri.Language.Parser
( program
) where

import Data.Either (partitionEithers)
import qualified Data.Text as T

import Biri.Language.AbstractSyntax
import Biri.Language.Lexer
}

%name program
%tokentype { ((Int, Int), Token) }
%error { parseError }
%monad { E } { thenE } { returnE }

%token
    indent      { (_, TIndent) }
    dedent      { (_, TDedent) }
    newline     { (_, TNewline) }
    eof         { (_, TEof) }
    int         { (_, TInt $$) }
    double      { (_, TDouble $$) }
    string      { (_, TString $$) }
    wildcard    { (_, TWildcard _) }
    ident       { (_, TIdent $$) }
    constructor { (_, TConstructor $$) }
    segment     { (_, TFixedSegment $$) }
    pattern     { (_, TPatternSegment $$) }
    seg_eps     { (_, TEmptySegment) }
    delete      { (_, TDelete) }
    get         { (_, TGet) }
    post        { (_, TPost) }
    put         { (_, TPut) }
    data        { (_, TData) }
    case        { (_, TCase) }
    of          { (_, TOf) }
    if          { (_, TIf) }
    else        { (_, TElse) }
    elif        { (_, TElif) }
    '->'        { (_, TRightArrow) }
    '<-'        { (_, TLeftArrow) }
    ':'         { (_, TColon) }
    '$'         { (_, TDollar) }
    '?'         { (_, TQuestion) }
    '!'         { (_, TBang) }
    '#'         { (_, THash) }
    '*'         { (_, TStar) }
    '='         { (_, TEquals) }
    '<'         { (_, TOpenAngle) }
    '>'         { (_, TCloseAngle) }
    '('         { (_, TOpenParen) }
    ')'         { (_, TCloseParen) }
    '['         { (_, TOpenBracket) }
    ']'         { (_, TCloseBracket) }
    '{'         { (_, TOpenCurlyBraces) }
    '}'         { (_, TCloseCurlyBraces) }
    '|'         { (_, TBar) }
    ','         { (_, TComma) }
    '\\'        { (_, TBackslash) }
%% 

Program
  : Resource Program    { let Program rs ds fns = $2 in Program ($1:rs) ds fns }
  | Data Program        { let Program rs ds fns = $2 in Program rs ($1:ds) fns }
  | Computation Program { let Program rs ds fns = $2 in Program rs ds ($1:fns) }
  | eof                 { Program [] [] [] }

Resource
  : URI newline Handlers { Resource $1 $3 }

URI
  : Segments { URI $1 }

Segments
  : seg_eps          { [] }
  | segment          { [Fixed $1] }
  | segment Segments { Fixed $1 : $2 }
  | pattern          { [Pattern $1] }
  | pattern Segments { Pattern $1 : $2 }

Handlers
  : Handler          { [$1] }
  | Handler Handlers { $1:$2 }

Handler
  : Method indent Instructions dedent { Handler $1 $3 }

Method
  : delete { Delete }
  | get    { Get }
  | post   { Post }
  | put    { Put }

Computation
  : ident Signature indent Instructions dedent { Component $1 $2 $4 }
  | ident Signature indent Expr dedent         { Function $1 $2 $4 }

Signature
  : ParamList ':' Type { Signature $1 $3 }

ParamList
  :  {- empty -}            { [] }
  | '(' Param ')' ParamList { $2 : $4 }

Param
  : ident ':' Type { Parameter $1 $3 }

Instructions
  : Instruction                      { [$1] }
  | Instruction newline Instructions { $1 : $3 }

Instruction
  : if IfStatement   { $2 }
  | ident '->' ident { Load $1 $3 }
  | ident '<-' Expr  { Store $1 $3 }

IfStatement
  : Expr indent Instructions dedent                                 { If $1 $3 }
  | Expr indent Instructions dedent else indent Instructions dedent { IfElse $1 $3 $7 }
  | Expr indent Instructions dedent elif IfStatement                { IfElse $1 $3 [$6] }

Data
  : data constructor Variables indent DataConstructors dedent { Data $2 $3 $5 } 

DataConstructors
  : DataConstructor                          { [$1] }
  | DataConstructor newline DataConstructors { $1 : $3 }

DataConstructor
  : constructor TypeList { ($1, $2) }

TypeList
  : {- empty -}         { [] }
  | TypeList AtomicType { $1 ++ [$2] }

Expr
  : Abstraction { $1 }

Abstraction
  : '\\' Lambda { TypedExpr $2 Nothing }
  | Case        { $1 }

Lambda
  : ident '->' Expr { Lambda $1 $3 }
  | ident Lambda    { Lambda $1 (TypedExpr $2 Nothing) }

Case
  : case Expr of indent Cases dedent          { TypedExpr (Case $2 $5) Nothing }
  | case Expr of indent Cases dedent ':' Type { TypedExpr (Case $2 $5) (Just $8) }
  | Application                               { $1 }

Cases
  : Pattern '->' Expr               { [($1, $3)] }
  | Pattern '->' Expr newline Cases { ($1, $3) : $5 }

Pattern
  : ident              { VariablePattern $1 }
  | wildcard           { WildcardPattern }
  | ConstructorPattern { $1 }

ConstructorPattern
  : constructor             { ConstructorPattern $1 [] }
  | constructor SubPatterns { ConstructorPattern $1 $2 }

SubPatterns
  : SubPatterns SubPattern { $1 ++ [$2] }
  | SubPattern             { [$1] }

SubPattern 
  : ident                      { VariablePattern $1 }
  | wildcard                   { WildcardPattern }
  | '(' ConstructorPattern ')' { $2 }

Application
  : Application Value          { TypedExpr (Application $1 $2) Nothing }
  | Application Value ':' Type { TypedExpr (Application $1 $2) (Just $4) }
  | Value                      { $1 }
  | Value ':' Type             { retype $1 $3 }

Value
  : Atomic       { TypedExpr $1 Nothing }
  | '(' Expr ')' { $2 }

Atomic
  : constructor { DataConstructor $1 }
  | ident       { Variable $1 }
  | '$' int     { Match $2 }
  | '?' ident   { Query $2 }
  | '#' ident   { Form $2 }
  | string      { Constant (LString $1) }
  | int         { Constant (LInt $1) }
  | double      { Constant (LDouble $1) }

Type
  : TypeApplication '->' Type { FunctionArrow $1 $3 }
  | TypeApplication           { $1 }

TypeApplication
  : TypeApplication AtomicType { TypeApplication $1 $2 }
  | AtomicType                 { $1 }

AtomicType
  : constructor       { TypeConstructor $1 }
  | ident             { TypeVariable $1 }
  | '(' Type ')'      { $2 }
  | '{' Interface '}' { uncurry ComponentType (partitionEithers $2) }

Interface
  : '?' Param               { [Left $2] }
  | '!' Param               { [Right $2] }
  | '?' Param ',' Interface { Left $2 : $4 }
  | '!' Param ',' Interface { Right $2 : $4 }

Variables
  : {- empty -}     { [] }
  | ident Variables { $1 : $2 }


{

type E = Either String
type LineNumber = Int


thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
       Right a -> k a
       Left e -> Left e

returnE :: a -> E a
returnE a = Right a

failE :: String -> E a
failE err = Left err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
      Right a -> Right a
      Left e -> Left e

parseError :: [((Int, Int), Token)] -> E a
parseError tokens = failE $ case tokens of
    [] -> "Unexpected end-of-file"
    ((line, column), tok):_ -> "Unexpected token '" ++ show tok ++ "' on line " ++ show line ++ ", column " ++ show column
}
