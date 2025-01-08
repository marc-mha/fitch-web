module Parser.Scope where

import Prelude (Unit, unit, (<$))

import Parsing (Parser)
import Parsing.String (string)

parseConsScopeLine :: Parser String Unit
parseConsScopeLine = unit <$ string "| "

parseAssScopeLine :: Parser String Unit
parseAssScopeLine = unit <$ string "|_"
