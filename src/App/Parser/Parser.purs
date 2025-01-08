module Parser where

import Prelude (const, ($), (<*))

import Model.Formula (Formula)
import Model.Proof (Proof)
import Model.Verification (Capture, Rule)
import Parser.Formula (parseFormula)
import Parser.Proof (parseLineProof, parseProof)
import Parser.Verification (parseJustification)

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Function (apply)
import Data.String (joinWith)
import Data.Tuple (Tuple)
import Parsing (Parser, runParser)
import Parsing.String (eof, parseErrorHuman)
import Parsing.String.Basic (skipSpaces)

readParser :: forall a. Parser String a -> String -> Maybe a
readParser p s = either (const Nothing) (apply Just) (runParser s (p <* eof))

readFormula :: String -> Maybe Formula
readFormula = readParser parseFormula

readLineProof :: String -> Maybe Proof
readLineProof = readParser parseLineProof

readProof :: String -> Either String Proof
readProof s = case runParser s (parseProof 0 <* skipSpaces <* eof) of
  Left err -> Left $ joinWith "\n" (parseErrorHuman s 20 err)
  Right res -> Right res

readRule :: String -> Maybe (Tuple Rule (Array Capture))
readRule = readParser parseJustification
