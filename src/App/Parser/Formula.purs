module Parser.Formula where

import Model.Formula (Formula(..))

import Prelude (Unit, unit, ($>), (*>), (<$), (<$>), (<*), (<<<))

import Control.Lazy (defer)
import Data.List (toUnfoldable)
import Data.Identity (Identity)
import Data.List.NonEmpty (toList)
import Data.String.CodeUnits (fromCharArray)
import Parsing (Parser)
import Parsing.Combinators (between, many, many1, (<|>))
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.String (string)
import Parsing.Token (alphaNum)

parseTrue :: Parser String Unit
parseTrue = string "top" $> unit

parseFalse :: Parser String Unit
parseFalse = string "bot" $> unit

parens :: forall a. Parser String a -> Parser String a
parens = between (string "(") (string ")")

-- Surely there is a better way????
parseAtom :: Parser String String
parseAtom = fromCharArray <<< toUnfoldable <<< toList <$> many1 alphaNum

prefix :: forall a. String -> (a -> a) -> Operator Identity String a
prefix name f = Prefix (f <$ string name)

postfix :: forall a. String -> (a -> a) -> Operator Identity String a
postfix name f = Postfix (f <$ string name)

binaryL :: forall a. String -> (a -> a -> a) -> Operator Identity String a
binaryL name f = Infix (f <$ string name <* (many (string " "))) AssocLeft

binaryR :: forall a. String -> (a -> a -> a) -> Operator Identity String a
binaryR name f = Infix (f <$ string name <* (many (string " "))) AssocRight

parseFormula :: Parser String Formula
parseFormula = defer \_ -> buildExprParser
  [
    -- [ prefix "¬" FNot
    -- , prefix "~" FNot
    -- ],
    [ binaryR "∧" FAnd
    , binaryR "&" FAnd
    , binaryR "∨" FOr
    , binaryR "|" FOr
    ]
  , [ binaryR "→" FImp
    , binaryR "->" FImp
    ]
  , [ binaryR "↔" FIff
    , binaryR "<->" FIff
    ]
  ]
  parseTerm

parseTerm :: Parser String Formula
parseTerm = defer \_ ->
  ( parens parseFormula
      <|> (FNot <$> ((string "~" <|> string "¬") *> parseTerm))
      <|> (FTrue <$ parseTrue)
      <|> (FFalse <$ parseFalse)
      <|> (FAtom <$> parseAtom)
  ) <* (many (string " "))
