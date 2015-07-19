{-# LANGUAGE OverloadedStrings #-}

module Biri.Language.Lexer
( tokenize
, Token(..)
) where

import Control.Applicative ((<$>), (<*), (<*>), (*>), (<|>))
import Control.Monad ((>=>), mfilter)
import Data.Attoparsec.Text
import Data.Char (isAlphaNum, isLower, isUpper, isSpace)
import Data.Function (on)
import Data.List (unfoldr)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (takeWhile)

import Biri.Language.AbstractSyntax

tokenize :: Text -> [((Int, Int), Token)]
tokenize = either error transform . parseOnly (many1 matchToken)

transform :: [Token] -> [((Int, Int), Token)]
transform = clean . finalize 0 . concat . filter ((/= TNewline) . snd . head)
          . indent [1] . toLines
          . filter (not . isHorizontalSpace . snd) . annotate 1 1
  where
    clean :: [((Int, Int), Token)] -> [((Int, Int), Token)]
    clean list = case list of
        []                                      -> []
        (_, TNewline) : (_, TIndent) : _        -> clean (tail list)
        (_, TNewline) : (_, TDedent) : _        -> clean (tail list)
        t@(_, TIndent) : (_, TNewline) : tokens -> t : clean tokens
        t@(_, TDedent) : (_, TNewline) : tokens -> t : clean tokens
        token : tokens                          -> token : clean tokens
     
    finalize :: Int -> [((Int, Int), Token)] -> [((Int, Int), Token)]
    finalize n tokens = case tokens of
        [] -> error "Biri.Language.Lexer.transform: empty token list"
        [t@(position, token)] -> (t:) . (++ [(position, TEof)]) $ case token of
            TIndent -> replicate (n + 1) (position, TDedent)
            TDedent -> replicate (n - 1) (position, TDedent)
            _       -> replicate n (position, TDedent)
        t@(_, token):rest -> (t:) . flip finalize rest $ case token of
            TIndent -> n + 1
            TDedent -> n - 1
            _       -> n    


    isHorizontalSpace :: Token -> Bool
    isHorizontalSpace token = case token of
        THorizontalSpace _ -> True
        _ -> False
    
    toLines :: [((Int, Int), Token)] -> [[((Int, Int), Token)]]
    toLines list = case break ((== TNewline) . snd) list of
        ([],[]) -> []
        (line, []) -> [line]
        (line, newline : tokens) -> (line ++ [newline]) : toLines tokens
    
    indent :: [Int] -> [[((Int, Int), Token)]] -> [[((Int, Int), Token)]]
    indent (offset:offsets) tokens = case tokens of
        [] -> []
        currentLine@(((line, column), _) : _) : remainingLines
            | offset == column -> currentLine : indent (offset:offsets) remainingLines
            | offset  < column -> (((line, 1), TIndent) : currentLine) : indent (column:offset:offsets) remainingLines
            | otherwise        -> [((line, 1), TDedent)] : indent offsets tokens
    
    annotate :: Int -> Int -> [Token] -> [((Int,Int), Token)]
    annotate line column list = case list of
        [] -> []
        token : tokens -> ((line, column), token) : case tokenWidth token of
            (0   , step  ) -> annotate line (column + step) tokens
            (step, offset) -> annotate (line + step) offset tokens
    
    tokenWidth :: Token -> (Int, Int)
    tokenWidth token = case token of
        TDelete -> (0,6)
        TGet -> (0,3)
        TPost -> (0,4)
        TPut -> (0,3)
        TData -> (0,4)
        TCase -> (0,4)
        TOf -> (0,2)
        TIf -> (0,2)
        TElse -> (0,4)
        TElif -> (0,4)
        TRightArrow -> (0,2)
        TLeftArrow -> (0,2)
        TColon -> (0,1)
        TDollar -> (0,1)
        TQuestion -> (0,1)
        TBang -> (0,1)
        THash -> (0,1)
        TStar -> (0,1)
        TEquals -> (0,1)
        TOpenAngle -> (0,1)
        TCloseAngle -> (0,1)
        TOpenParen -> (0,1)
        TCloseParen -> (0,1)
        TOpenBracket -> (0,1)
        TCloseBracket -> (0,1)
        TOpenCurlyBraces -> (0,1)
        TCloseCurlyBraces -> (0,1)
        TBar -> (0,1)
        TBackslash -> (0,1)
        TComma -> (0,1)
        TIndent -> (0,4)
        TDedent -> (0,4)
        TNewline -> (1,1)
        TEof -> (0,0)
        THorizontalSpace n -> (0, n)
        TWildcard n -> (0, 1 + n)
        TIdent (Identifier t) -> (0, T.length t)
        TConstructor (Constructor t) -> (0, T.length t)
        TEmptySegment -> (0,1)
        TFixedSegment t -> (0, 1 + T.length t)
        TPatternSegment (Constructor t) -> (0, 3 + T.length t)
        TInt n -> (0, length (show n))
        TDouble d -> (0, length (show d))
        TString t -> case T.lines t of
          [_] -> (0, 2 + T.length t)
          texts -> (length texts - 1, 1 + T.length (last texts))
          

data Token
    = TDelete
    | TGet
    | TPost
    | TPut
    | TData
    | TCase
    | TOf
    | TIf
    | TElse
    | TElif
    | TRightArrow
    | TLeftArrow
    | TColon
    | TDollar
    | TQuestion
    | TBang
    | THash
    | TStar
    | TEquals
    | TOpenAngle
    | TCloseAngle
    | TOpenParen
    | TCloseParen
    | TOpenBracket
    | TCloseBracket
    | TOpenCurlyBraces
    | TCloseCurlyBraces
    | TBar
    | TBackslash
    | TComma
    | TIndent
    | TDedent
    | TNewline
    | TEof
    | THorizontalSpace Int
    | TWildcard Int
    | TIdent Identifier
    | TConstructor Constructor
    | TEmptySegment
    | TFixedSegment Text
    | TPatternSegment Constructor
    | TInt Int
    | TDouble Double
    | TString Text
    deriving (Show, Eq)

matchToken :: Parser Token
matchToken = choice
    [ const TNewline <$> ("--" *> takeWhile isEndOfLine)
    , const TRightArrow <$> "->"
    , const TLeftArrow <$> "<-"
    , const TColon <$> char ':'
    , const TComma <$> char ','
    , const TDollar <$> char '$'
    , const TQuestion <$> char '?'
    , const TBang <$> char '!'
    , const THash <$> char '#'
    , const TStar <$> char '*'
    , const TEquals <$> char '='
    , const TOpenAngle <$> char '<'
    , const TCloseAngle <$> char '>'
    , const TOpenParen <$> char '('
    , const TCloseParen <$> char ')'
    , const TOpenBracket <$> char '['
    , const TCloseBracket <$> char ']'
    , const TOpenCurlyBraces <$> char '{'
    , const TCloseCurlyBraces <$> char '}'
    , const TBar <$> char '|'
    , const TBackslash <$> char '\\'
    , const TNewline <$> endOfLine
    , THorizontalSpace . countSpaces <$> takeWhile1 isHorizontalSpace
    , TWildcard . T.length <$> ("_" *> takeWhile isSpace)
    , const TDelete <$> textual "DELETE"
    , const TGet <$> textual "GET"
    , const TPost <$> textual "POST"
    , const TPut <$> textual "PUT"
    , const TData <$> textual "data"
    , const TCase <$> textual "case"
    , const TOf <$> textual "of"
    , const TIf <$> textual "if"
    , const TElse <$> textual "else"
    , const TElif <$> textual "elif"
    , TConstructor <$> constructorToken
    , TIdent <$> identToken
    , TPatternSegment <$> ("/<" *> constructorToken <* char '>')
    , TFixedSegment <$> ("/" *> takeWhile1 ((&&) <$> (/= '/') <*> not . isEndOfLine))
    , const TEmptySegment <$> char '/'
    , TString <$> stringToken
    , TInt <$> decimal
    , TDouble <$> double
    ]

stringToken :: Parser Text
stringToken = T.concat <$> (char '"' *> many1 (choice [T.singleton <$> notChar '"', "\\\""]) <* char '"')

constructorToken :: Parser Constructor
constructorToken = (Constructor .) . T.cons <$> satisfy isUpper <*> takeWhile isAlphaNum

identToken :: Parser Identifier
identToken = (Identifier .) . T.cons <$> satisfy isLower <*> takeWhile isAlphaNum

countSpaces :: Text -> Int
countSpaces = sum . map (\c -> if c == '\t' then 8 else 1) . T.unpack

textual :: Text -> Parser Text
textual t = string t <* notFollowedBy isAlphaNum

notFollowedBy :: (Char -> Bool) -> Parser (Maybe Char)
notFollowedBy p = mfilter (maybe True (not . p)) peekChar
