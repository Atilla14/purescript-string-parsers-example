module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array (replicate)
import Data.Either (Either(..))
import Data.List (List, foldMap)
import Data.String.Common as SC
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.StringParser (runParser, Parser(..))
import Text.Parsing.StringParser.CodeUnits (string, char, anyDigit, anyChar)
import Text.Parsing.StringParser.Combinators (many, sepBy)
import Text.Parsing.StringParser.Expr (Assoc(..), Operator(..), buildExprParser)

{--parser :: Parser (List String)--}
{--parser = sepBy (string "hello" <|> string "sathya") $ char ' '--}

testParser :: Parser (List String)
testParser = (many $ string "a") <> (many $ string "b")

digit :: Parser Int
digit = string "0" $> 0
    <|> string "1" $> 1
    <|> string "2" $> 2
    <|> string "3" $> 3
    <|> string "4" $> 4
    <|> string "5" $> 5
    <|> string "6" $> 6
    <|> string "7" $> 7
    <|> string "8" $> 8
    <|> string "9" $> 9

exprParser :: Parser Int
exprParser = buildExprParser [ [Infix (string "/" >>= \_ -> pure div) AssocRight]
                             , [Infix (string "*" >>= \_ -> pure mul) AssocRight]
                             , [Infix (string "-" >>= \_ -> pure sub) AssocRight]
                             , [Infix (string "+" >>= \_ -> pure add) AssocRight]
                             ] digit



main :: Effect Unit
main = do
  {--let input = SC.joinWith "" $ (replicate 10 "a") <> (replicate 50 "b")--}
  let input = "1+2*3-5+9"
  log $ "Input:\n" <> input
  case runParser exprParser input of
       Right val -> log $ "Success:\n" <> show val
       Left err -> log $ "Error:\n" <>  show err
  log "Hello sailor!"
