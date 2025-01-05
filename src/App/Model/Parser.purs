module Parser where

import Data.Array hiding (many, reverse, toUnfoldable)
import Data.Either
import Data.Maybe hiding (optional)
import Formula
import Parsing
import Parsing.Combinators
import Parsing.Expr
import Parsing.String
import Prelude hiding (between)
import Proof

import Control.Lazy (defer)
import Data.Function (apply)
import Data.Identity (Identity(..))
import Data.List
import Data.List.NonEmpty (toList)
import Data.String.CodeUnits (fromCharArray)
import Parsing.String.Basic (skipSpaces)
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
binaryL name f = Infix (f <$ string name <* skipSpaces) AssocLeft

binaryR :: forall a. String -> (a -> a -> a) -> Operator Identity String a
binaryR name f = Infix (f <$ string name <* skipSpaces) AssocRight

parseFormula :: Parser String Formula
parseFormula = defer \_ -> buildExprParser
  [ [ prefix "¬" FNot
    , prefix "~" FNot
    ]
  , [ binaryR "∧" FAnd
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
      <|> (FTrue <$ parseTrue)
      <|> (FFalse <$ parseFalse)
      <|> (FAtom <$> parseAtom)
  ) <* skipSpaces

parseConclusion :: Parser String Conclusion
parseConclusion = defer \_ -> parens (SubProof <$> parseProof) <|> (SubFormula <$> parseFormula)

parseConclusions :: Parser String (List Conclusion)
parseConclusions = defer \_ -> reverse <$> (parseConclusion `sepBy` (string "," <* skipSpaces))

parseProof :: Parser String Proof
parseProof = defer \_ -> do
  ass <- optionMaybe parseFormula
  string "|-" *> skipSpaces
  concs <- parseConclusions
  pure (Proof (maybe FTrue identity ass) concs)

readParser :: forall a. Parser String a -> String -> Maybe a
readParser p s = either (const Nothing) (apply Just) (runParser s (p <* eof))

readFormula :: String -> Maybe Formula
readFormula = readParser parseFormula

readProof :: String -> Maybe Proof
readProof = readParser parseProof
