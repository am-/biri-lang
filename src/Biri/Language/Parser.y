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

Program : Resource             { Program [$1] [] [] }
        | Resource Program     { let Program rs ds fns = $2 in Program ($1:rs) ds fns }
        | Data                 { Program [] [$1] [] }
        | Data Program         { let Program rs ds fns = $2 in Program rs ($1:ds) fns }
        | Computation          { Program [] [] [$1] }
        | Computation Program  { let Program rs ds fns = $2 in Program rs ds ($1:fns) }

Resource : URI newline Handlers { Resource $1 $3 }

URI : Segments { URI $1 }

Segments : seg_eps          { [] }
         | segment          { [Fixed $1] }
         | segment Segments { Fixed $1 : $2 }
         | pattern          { [Pattern $1] }
         | pattern Segments { Pattern $1 : $2 }

Handlers : Handler          { [$1] }
         | Handler Handlers { $1:$2 }

Handler : Method indent Instructions dedent  { Handler $1 $3 }

Method : delete { Delete }
       | get    { Get }
       | post   { Post }
       | put    { Put }

Computation : ident Signature indent Instructions dedent { Component $1 $2 $4 }
            | ident Signature indent Expr dedent         { Function $1 $2 $4 }

Signature : ParamList ':' Type { Signature $1 $3 }

ParamList :  {- empty -}            { [] }
          | '(' Param ')' ParamList { $2 : $4 }

Param : ident ':' Type { Parameter $1 $3 }

Instructions : Instruction                      { [$1] }
             | Instruction newline Instructions { $1 : $3 }

Instruction : if IfStatement   { $2 }
            | ident '->' ident { Load $1 $3 }
            | ident '<-' Expr  { Store $1 $3 }

IfStatement : Expr indent Instructions dedent                                 { If $1 $3 }
            | Expr indent Instructions dedent else indent Instructions dedent { IfElse $1 $3 $7 }
            | Expr indent Instructions dedent elif IfStatement                { IfElse $1 $3 [$6] }

Data : data constructor Variables indent TypeConstructors dedent { Data $2 $3 $5 } 

TypeConstructors : constructor Types                          { [TypeConstructor $1 $2] }
                 | constructor Types newline TypeConstructors { TypeConstructor $1 $2 : $4 }


Expr : '\\' Lambda                      { $2 }
     | case Expr of indent Cases dedent { Case $2 $5 }
     | Expression                       { $1 }
     | Expression Expressions           { foldl Application (Application $1 (head $2)) (tail $2) }

Lambda : ident '->' Expr { Lambda $1 $3 }
       | ident Lambda    { Lambda $1 $2 }

Cases : Pattern '->' Expr               { [($1, $3)] }
      | Pattern '->' Expr newline Cases { ($1, $3) : $5 }

Patterns : {- empty -}      { [] }
         | Pattern Patterns { $1 : $2 }

Pattern : constructor Patterns { ConstructorPattern $1 $2 }
        | ident                { VariablePattern $1 }
        | wildcard             { WildcardPattern }
        | '(' Pattern ')'      { $2 }

Expressions : Expression             { [$1] }
            | Expression Expressions { $1 : $2 }

Expression : ident                   { Variable $1 }
           | '$' int                 { Match $2 }
           | '?' ident               { Query $2 }
           | '#' ident               { Form $2 }
           | constructor             { DataConstructor $1 [] }
           | constructor Expressions { DataConstructor $1 $2 }
           | string                  { Constant (LString $1) }
           | int                     { Constant (LInt $1) }
           | double                  { Constant (LDouble $1) }
           | '(' Expr ')'            { $2 }

Types : {- empty -}        { [] }
      | constructor Types  { TypeConstructor $1 [] : $2 }
      | ident Types        { TypeVariable $1 : $2 }
      | '(' Type ')' Types { $2 : $4 }

Type : Typ           { $1 }
     | Type '->' Typ { FunctionArrow $1 $3 }

Typ : constructor Types { TypeConstructor $1 $2 }
    | ident             { TypeVariable $1 }
    | '(' Type ')'      { $2 }
    | '{' Interface '}' { uncurry ComponentType (partitionEithers $2) }

Interface : '?' Param               { [Left $2] }
          | '!' Param               { [Right $2] }
          | '?' Param ',' Interface { Left $2 : $4 }
          | '!' Param ',' Interface { Right $2 : $4 }

Variables : {- empty -}     { [] }
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
