module Parser.Proof where

import Prelude (bind, discard, identity, pure, ($), (*>), (+), (<$>), (<*), (==))

import Model.Proof (Conclusion(..), Proof(..))
import Model.Formula (Formula(..))
import Parser.Formula (parens, parseFormula)
import Parser.Scope (parseAssScopeLine, parseConsScopeLine)

import Control.Lazy (defer)
import Control.MonadPlus (guard)
import Data.List (List, length, reverse)
import Data.Maybe (maybe)
import Parsing (Parser)
import Parsing.Combinators (many, optionMaybe, sepBy, try, (<|>))
import Parsing.String (string)

parseScopedFormula :: Int -> Parser String Formula
parseScopedFormula n = do
  s :: Int <- length <$> many (parseConsScopeLine)
  guard (s == n)
  parseFormula

parseConclusions :: Int -> Parser String (List Conclusion)
parseConclusions n = do
  -- NOTE: Need the tries as what distinguishes each case is consumed
  many $
    ( (try (SubProof <$> parseProof n))
        <|>
          try ((SubFormula <$> parseScopedFormula n) <* (string "\n"))
    )

parseProof :: Int -> Parser String Proof
parseProof n = do
  s :: Int <- length <$> many (parseConsScopeLine)
  parseAssScopeLine
  guard (s == n)
  -- Try to parse a formula, otherwise treat it as the empty (true) assumption
  ass <- maybe FTrue (identity) <$> (optionMaybe parseFormula)
  _ <- string "\n"
  concs <- reverse <$> parseConclusions (s + 1)
  pure (Proof ass concs)

parseLineConclusion :: Parser String Conclusion
parseLineConclusion = defer \_ -> parens (SubProof <$> parseLineProof) <|> (SubFormula <$> parseFormula)

parseLineConclusions :: Parser String (List Conclusion)
parseLineConclusions = defer \_ -> reverse <$> (parseLineConclusion `sepBy` (string "," <* (many (string " "))))

parseLineProof :: Parser String Proof
parseLineProof = defer \_ -> do
  ass <- optionMaybe parseFormula
  _ <- string "|-" *> (many (string " "))
  concs <- parseLineConclusions
  pure (Proof (maybe FTrue identity ass) concs)
