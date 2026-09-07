module LGPT.TUI where


import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import LGPT.Helpers (Parser, prompt, runStart)
import LGPT.Numbers (parseExpr)
import Data.Time
import Data.Char (isDigit)

--------------------------------------------------------------------------------
-- Memory,requests and REPL

-- Memory stores both remembered facts and the most recent arithmetic answer.
type Facts = [(String, String)]
type Memory = (Facts, Maybe String)

-- runREPL starts the chatbot with empty facts and no previous arithmetic answer.
-- A recursive loop is used instead of a plain forever loop because memory has to
-- be carried from one interaction to the next.
runREPL :: IO ()
runREPL = loop ([], Nothing)
  -- The loop reads input, parses it, handles it, then repeats with updated memory.
  -- This keeps the IO layer small and leaves most of the logic to helper functions.
  where
    loop :: Memory -> IO ()
    loop memory = do
      putStr prompt
      input <- getLine
      let req = readRequest input
      memory' <- respondToWithMemory memory req
      loop memory'

--------------------------------------------------------------------------------
-- Parsing and responding to requests

data Request
  = Hello
  | Unknown
  | Today
  | Tomorrow
  | HowLong String String String
  | MathExpr String
  | RememberName String String
  | TellMe String
  deriving (Eq, Ord, Show)

-- Failed parses are treated as Unknown so invalid input does not crash the chatbot.
readRequest :: String -> Request
readRequest str =
  case parse parseRequest "<stdin>" str of
    Left _ -> Unknown
    Right req -> req

-- parseRequest is split into small parsers rather than one large parser.
-- This makes each supported sentence form easier to understand and change.
parseRequest :: Parser Request
parseRequest = choice
  [ parseHello
  , parseToday
  , parseTomorrow
  , parseHowLong
  , parseMathExpr
  , parseRememberName
  , parseTellMe
  ]

-- Parse the Hello request.
parseHello :: Parser Request
parseHello = string "Hello" >> eof >> pure Hello

-- Parse a request asking for today's day of the week.
parseToday :: Parser Request
parseToday = string "What day is it?" >> eof >> pure Today

-- Parse a request asking for tomorrow's day of the week.
parseTomorrow :: Parser Request
parseTomorrow = string "What day is it tomorrow?" >> eof >> pure Tomorrow

-- Parse a yyyy-mm-dd date into separate parts.
-- The date is kept as strings here because validation is handled later when responding.
parseHowLong :: Parser Request
parseHowLong = do
  string "How long ago was "
  year <- count 4 digitChar
  char '-'
  month <- count 2 digitChar
  char '-'
  day <- count 2 digitChar
  char '?'
  eof
  pure (HowLong year month day)

--- Parse the raw arithmetic text after "What is ".
-- The expression is not evaluated here because "that" depends on memory, so the
-- actual evaluation is delayed until the response stage.
parseMathExpr :: Parser Request
parseMathExpr = do
  string "What is "
  raw <- many (satisfy (/= '?'))
  char '?'
  eof
  pure (MathExpr raw)

-- Parse a fact to be remembered.
-- manyTill is used so names can contain spaces.
parseRememberName :: Parser Request
parseRememberName = do
  string "Remember that "
  name <- manyTill anySingle (try (string " is "))
  thing <- manyTill anySingle (char '.')
  eof
  pure (RememberName name thing)

-- Parse a request asking for a remembered fact.
parseTellMe :: Parser Request
parseTellMe = do
  string "Tell me about "
  name <- manyTill anySingle (char '.')
  eof
  pure (TellMe name)

--------------------------------------------------------------------------------
-- Responding to requests

-- Response handling updates memory only when needed, while unchanged cases pass memory through.
respondToWithMemory :: Memory -> Request -> IO Memory
respondToWithMemory memory req =
  case req of
    Hello -> respondHello memory
    Today -> respondToday memory
    Tomorrow -> respondTomorrow memory
    HowLong year month day -> respondHowLong memory year month day
    MathExpr raw -> respondMathExpr memory raw
    RememberName name thing -> respondRememberName memory name thing
    TellMe name -> respondTellMe memory name
    Unknown -> respondUnknown memory

-- Reply to a greeting without changing memory.
respondHello :: Memory -> IO Memory
respondHello memory = do
  putStrLn "Hi there!"
  pure memory

-- Today's weekday is computed here rather than in the parser because it depends
-- on the current outside world and therefore belongs in IO.
respondToday :: Memory -> IO Memory
respondToday memory = do
  today <- utctDay <$> getCurrentTime
  putStrLn ("Today is " ++ show (dayOfWeek today) ++ ".")
  pure memory

-- Tomorrow is computed by adding one day to today's date.
respondTomorrow :: Memory -> IO Memory
respondTomorrow memory = do
  today <- utctDay <$> getCurrentTime
  let tomorrow = addDays 1 today
  putStrLn ("Tomorrow is " ++ show (dayOfWeek tomorrow) ++ ".")
  pure memory

-- The date string is validated here using the time library.
-- Doing the validation here keeps the parser simple and avoids partial functions.
respondHowLong :: Memory -> String -> String -> String -> IO Memory
respondHowLong memory year month day = do
  today <- utctDay <$> getCurrentTime
  let dateStr = year ++ "-" ++ month ++ "-" ++ day
  let given = parseTimeM True defaultTimeLocale "%F" dateStr :: Maybe Day
  case given of
    Nothing ->
      putStrLn "Invalid date."
    Just d -> do
      let daysAgo = diffDays today d
      putStrLn (dateStr ++ " was " ++ show daysAgo ++ " days ago.")
  pure memory

-- Arithmetic is handled here because this is the point where memory is available.
-- That makes it possible to replace "that" before running the real expression parser.
respondMathExpr :: Memory -> String -> IO Memory
respondMathExpr (facts, previousAnswer) raw =
  case prepareExpr raw previousAnswer of
    Nothing -> do
      putStrLn "I haven't evaluated anything yet."
      pure (facts, previousAnswer)
    Just exprText ->
      case parse parseExpr "expr" exprText of
        Left _ -> do
          putStrLn "Sorry i dont understand that"
          pure (facts, previousAnswer)
        Right answer -> do
          putStrLn ("The answer is " ++ answer ++ ".")
          pure (facts, Just answer)

-- New facts are added to the front of the list.
-- This means if something is remembered twice, the most recent fact is found first.
respondRememberName :: Memory -> String -> String -> IO Memory
respondRememberName (facts, previousAnswer) name thing = do
  putStrLn "Okay."
  pure ((name, thing) : facts, previousAnswer)

-- Look up a remembered fact and respond accordingly.
respondTellMe :: Memory -> String -> IO Memory
respondTellMe (facts, previousAnswer) name =
  case lookup name facts of
    Nothing -> do
      putStrLn ("Sorry, I don't know anything about " ++ name ++ ".")
      pure (facts, previousAnswer)
    Just thing -> do
      putStrLn ("Sure - " ++ name ++ " is " ++ thing ++ ".")
      pure (facts, previousAnswer)

-- Fallback response for unrecognised input.
respondUnknown :: Memory -> IO Memory
respondUnknown memory = do
  putStrLn "Sorry i dont understand that"
  pure memory

--------------------------------------------------------------------------------
-- Helpers

-- If the user writes "that", the expression needs the previous arithmetic answer.
-- This helper keeps that replacement logic separate from the response function.
prepareExpr :: String -> Maybe String -> Maybe String
prepareExpr raw previousAnswer
  | elem "that" (words raw) =
      case previousAnswer of
        Nothing -> Nothing
        Just answer -> Just (replaceThat raw answer)
  | otherwise = Just raw

-- replaceThat works word-by-word so only the exact word "that" is replaced.
-- This is safer than doing a raw substring replacement.
replaceThat :: String -> String -> String
replaceThat raw answer = unwords (go (words raw))
  where
    answerWords = words answer
    -- Walk through the expression word by word performing replacement.
    go [] = []
    go ("that" : xs) = answerWords ++ go xs
    go (x : xs) = x : go xs