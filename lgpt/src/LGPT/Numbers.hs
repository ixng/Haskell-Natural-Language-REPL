module LGPT.Numbers where

import LGPT.Helpers
import Control.Applicative (some)
import Text.Megaparsec (choice, try , eof, parse, (<|>))
import Text.Megaparsec.Char

--------------------------------------------------------------------------------
--Print an integer in longhand form.

-- Convert an integer result back into the spoken form expected by the chatbot.
printLonghand :: Int -> String
printLonghand n
  | n >  1000000  = "an unfathomably large number"
  | n == 1000000  = "one million"
  | n == 0        = "zero"
  | n >= 1000     = let
      thouStr   = printLonghand (n `div` 1000) ++ " thousand"
      and       = if n `mod` 1000 >= 100 then " " else " and "
      hundStr   = printLonghand (n `mod` 1000)
      in if n `mod` 1000 == 0
         then thouStr
         else thouStr ++ and ++ hundStr
  | n >= 100 = let
      hundStr = printLonghand (n `div` 100) ++ " hundred"
      tensStr = printLonghand (n `mod` 100)
      in if n `mod` 100 == 0
         then hundStr
         else hundStr ++ " and " ++ tensStr
  | n >= 20 = let
      tensStr = tens !! ((n `div` 10) - 2)
      hyphen  = if n `mod` 10 /= 0 then "-" else ""
      unitStr = ("" : oneToNine) !! (n `mod` 10)
      in tensStr ++ hyphen ++ unitStr
  | n > 0 = ("" : oneToNine ++ tenToTwenty) !! n
  | otherwise = "a negative number"

--------------------------------------------------------------------------------
--Parse an integer in longhand form.

-- Reuse a dedicated number parser so arithmetic parsing does not duplicate this logic.
parseLonghand :: Parser Int
parseLonghand = choice
  [ string "one million" >> pure 1000000
  , try parseSubMillion
  , try parseSubThousand
  , try parseSubHundred
  , try parseUnit
  , string "zero" >> pure 0
  ]
  where
    -- Parse values in the thousands or hundred-thousands range.
    parseSubMillion :: Parser Int
    parseSubMillion = do
      th <- choice [try parseSubThousand, parseSubHundred]
      string " thousand"
      rest <- choice
        [ string " and " >> parseSubHundred
        , string " " >> parseSubThousand
        , pure 0
        ]
      pure $ th * 1000 + rest
    -- Parse values in the hundreds range.
    parseSubThousand :: Parser Int
    parseSubThousand = do
      h <- parseUnit
      string " hundred"
      rest <- (string " and " >> parseSubHundred) <|> pure 0
      pure $ h * 100 + rest
    -- Parse values below one hundred.
    parseSubHundred :: Parser Int
    parseSubHundred = choice
      [ do
          tensValue <- choice $ zipWith (\v s -> string s >> pure v) [20,30..90] tens
          rest <- (string "-" >> parseUnit) <|> pure 0
          pure $ tensValue + rest
      , parseTenToTwenty
      , parseUnit
      ]
    -- Parse the irregular ten-to-nineteen words.
    parseTenToTwenty :: Parser Int
    parseTenToTwenty =
      choice $ zipWith (\v s -> string s >> pure v) [10..19] tenToTwenty
    -- Parse a single unit word.
    parseUnit :: Parser Int
    parseUnit =
      choice $ zipWith (\v s -> string s >> pure v) [1..9] oneToNine

--------------------------------------------------------------------------------
-- Parse and evaluate arithmetic expressions left-to-right.

-- parseExpr returns a String because the chatbot expects the final answer in
-- longhand English rather than as a raw integer.
parseExpr :: Parser String
parseExpr = printLonghand <$> (parseArithmetic <* eof)

-- parseArithmetic first reads the opening number, then the rest of the operations.
-- Splitting the parser this way makes the evaluation order easier to follow.
parseArithmetic :: Parser Int
parseArithmetic = do
  first <- parseLonghand
  rest <- parseRest
  pure (applyOperations first rest)

-- parseRest collects the remaining operator-number pairs.
-- This keeps parsing and evaluation separate, which makes the structure clearer.
parseRest :: Parser [(Int -> Int -> Int, Int)]
parseRest =
  (do
      next <- try parseOpAndNumber
      more <- parseRest
      pure (next : more))
  <|> pure []

-- Parse one operator followed by the next number.
parseOpAndNumber :: Parser (Int -> Int -> Int, Int)
parseOpAndNumber = do
  space
  op <- parseOperator
  space
  n <- parseLonghand
  pure (op, n)

-- Operators are parsed as functions so they can be applied uniformly later.
parseOperator :: Parser (Int -> Int -> Int)
parseOperator = parseAdd <|> parseMinus <|> parseTimes

-- Parse "plus" and return the addition function.
parseAdd :: Parser (Int -> Int -> Int)
parseAdd = string "plus" >> pure (+)

-- Parse "minus" and return the subtraction function.
parseMinus :: Parser (Int -> Int -> Int)
parseMinus = string "minus" >> pure (-)

-- Parse "times" and return the multiplication function.
parseTimes :: Parser (Int -> Int -> Int)
parseTimes = string "times" >> pure (*)

-- Apply the parsed operations left-to-right as required by the specification.
applyOperations :: Int -> [(Int -> Int -> Int, Int)] -> Int
applyOperations = foldl applyStep

-- Apply one parsed operator-number pair to the current accumulator.
applyStep :: Int -> (Int -> Int -> Int, Int) -> Int
applyStep acc (op, n) = op acc n

--------------------------------------------------------------------------------
-- Helpers used for both parsing and printing

-- Words for single-digit values.
oneToNine :: [String]
oneToNine =
  [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]

-- Words for the irregular values ten to nineteen.
tenToTwenty :: [String]
tenToTwenty =
  [ "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen"
  , "sixteen", "seventeen", "eighteen", "nineteen"
  ]

-- Words for the tens multiples from twenty to ninety.
tens :: [String]
tens =
  [ "twenty", "thirty", "forty", "fifty"
  , "sixty", "seventy", "eighty", "ninety"
  ]
