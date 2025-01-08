module Parser.Verification where

import Prelude (bind, discard, identity, map, pure, ($), (*>), (<$>), (<=), (<>), (>=))

import Model.Inference (andElim, andIntro, iffElim, iffIntro, impElim, impIntro, notElim, notIntro, orElim, orIntro)
import Model.Verification (Capture(..), Rule(..))

import Control.MonadPlus (guard)
import Data.Foldable (lookup)
import Data.List (List, toUnfoldable)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..), fst, uncurry)
import Parsing (Parser)
import Parsing.Combinators (choice, many, optionMaybe, sepBy, try, (<|>))
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, skipSpaces)

parseCapture :: Parser String Capture
parseCapture = ((try (uncurry Lines <$> pos2)) <|> Line <$> pos)
  where
  pos :: Parser String Int
  pos = do
    n <- intDecimal
    guard (n >= 0)
    pure n

  pos2 :: Parser String (Tuple Int Int)
  pos2 = do
    n <- pos
    _ <- (many (string " ")) *> string "-" *> (many (string " "))
    m <- pos
    guard (n <= m)
    pure (Tuple n m)

parseCaptures :: Parser String (List Capture)
parseCaptures = parseCapture `sepBy` (string "," *> (many (string " ")))

ruleTable :: Array (Tuple String Rule)
ruleTable =
  [ Tuple "A" Ass
  , Tuple "R" Reit
  , Tuple "∧I" (Inf "∧I" andIntro)
  , Tuple "∧E" (Inf "∧E" andElim)
  , Tuple "∨I" (Inf "∨I" orIntro)
  , Tuple "∨E" (Inf "∨E" orElim)
  , Tuple "¬I" (Inf "¬I" notIntro)
  , Tuple "¬E" (Inf "¬E" notElim)
  , Tuple "→I" (Inf "→I" impIntro)
  , Tuple "→E" (Inf "→E" impElim)
  , Tuple "↔I" (Inf "↔I" iffIntro)
  , Tuple "↔E" (Inf "↔E" iffElim)
  ]

readTable :: Array (Tuple String Rule)
readTable =
  [ Tuple "&I" (Inf "∧I" andIntro)
  , Tuple "&E" (Inf "∧E" andElim)
  , Tuple "|I" (Inf "∨I" orIntro)
  , Tuple "|E" (Inf "∨E" orElim)
  , Tuple "~I" (Inf "¬I" notIntro)
  , Tuple "~E" (Inf "¬E" notElim)
  , Tuple "->I" (Inf "→I" impIntro)
  , Tuple "->E" (Inf "→E" impElim)
  , Tuple "<->I" (Inf "↔I" iffIntro)
  , Tuple "<->E" (Inf "↔E" iffElim)
  ]

validRules :: Array String
validRules = map fst ruleTable <> map fst readTable

parseRule :: Parser String Rule
parseRule = do
  r :: String <- choice $ (map string) validRules
  let mR = lookup r ruleTable <|> lookup r readTable
  pure (maybe Ass identity mR)

parseJustification :: Parser String (Tuple Rule (Array Capture))
parseJustification = do
  rule <- parseRule
  caps <- optionMaybe $ do
    string "," *> skipSpaces
    toUnfoldable <$> parseCaptures
  pure (Tuple rule (maybe [] identity caps))
