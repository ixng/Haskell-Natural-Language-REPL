module LGPT.Helpers where

import Text.Megaparsec
import Data.Void (Void)
import System.Console.ANSI
import System.IO
import Control.Monad (when)

-- | Our Parser monad is a specialisation of the complex "Parsec" type 
-- so that it works on strings. 
type Parser = Parsec Void String


-- | The prompt to show the user when asking for input.
-- Feel free to change this to something else.
prompt :: String
prompt = "λ> "

-- | Set up the interactive terminal session.
runStart :: IO ()
runStart = do
  isTTY <- hIsTerminalDevice stdin

  -- Only do the interactive setup when running interactively.
  when isTTY $ do

    -- Clear away the stack build details.
    clearScreen

    -- We set this so the prompt will always print out right away.
    hSetBuffering stdout NoBuffering

    -- Change text to magenta
    setSGR [SetColor Foreground Vivid Magenta]

    -- Print a nice picture
    mapM_ putStrLn
      [ "    ___     _____ _____ _______  "
      , "    \\  \\   / ____|  __ \\__   __| "
      , "     \\  \\ | |  __| |__) | | |    "
      , "      >  \\| | |_ |  ___/  | |    "
      , "     / /\\ | |__| | |      | |    "
      , "    /_/  \\_\\_____|_|      |_|    "
      , "The Lightly Generalised Parsing Task"
      , ""
      ]

    -- Unset the colour
    setSGR [Reset]

    -- And now we are ready!