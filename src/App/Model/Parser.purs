module Parser where

import Data.Array hiding (many, reverse, toUnfoldable, length, dropWhile)
import Data.Either
import Data.List hiding (many)
import Data.Maybe hiding (optional)
import Formula
import Inference
import Parsing
import Parsing.Combinators
import Parsing.Expr
import Parsing.String
import Prelude hiding (between)
import Proof
import Verification

import Control.Lazy (defer)
import Control.MonadPlus (guard)
import Data.Foldable (lookup)
import Data.Function (apply)
import Data.Identity (Identity)
import Data.List.NonEmpty (toList)
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..), fst, uncurry)
import Parsing.String.Basic (intDecimal, skipSpaces, space)
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

parseConsScopeLine :: Parser String Unit
parseConsScopeLine = unit <$ string "| "

parseAssScopeLine :: Parser String Unit
parseAssScopeLine = unit <$ string "|_"

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
    pure (Tuple n m)

parseCaptures :: Parser String (List Capture)
parseCaptures = parseCapture `sepBy` (string "," *> (many (string " ")))

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

readRule :: String -> Maybe (Tuple Rule (Array Capture))
readRule = readParser parseJustification
