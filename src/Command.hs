module Command 
  ( Cmd (..)
  , runCmd
  ) where

--------------------
-- Import
--------------------

import           Data.Monoid((<>))
import           Options.Applicative
import           Constant
import           Type

--------------------
-- Data
--------------------

--------------------
-- Data definition
--------------------

--------------------
-- Function
--------------------

-- | parserCmdInfo adds descriptions and help to `parseCmd`
parseCmdInfo :: ParserInfo Cmd
parseCmdInfo = info 
    (helper <*> parseCmd)
    (fullDesc 
    <> progDesc "hcc - a command-line program to annotate source code written in Haskell programming language." 
    <> header "hcc - Haskell Comment Command (HCC) line program."
    )

parseCmd :: Parser Cmd
parseCmd = hsubparser 
    (  parseAddSection 
    <> parseToggleLine 
    <> parseToggleHaddock 
    <> parseToggleBlock 
    )

-- | parseToggleLine produces a parser for `toggle-line` command
parseAddSection :: Mod CommandFields Cmd
parseAddSection = command "add-section" parserInfo
  where
    parserInfo = info 
        (AddSection <$> pure sectionPrefix)
        (progDesc "Add a section around the selected text.")
                      
-- | parseToggleLine produces a parser for `toggle-line` command
parseToggleLine :: Mod CommandFields Cmd
parseToggleLine = command "toggle-line" parserInfo
  where
    parserInfo = info 
        (ToggleLine <$> pure lineCommentPrefix)  
        (progDesc " Toggle line comment on/off over the current line.")
                      
-- | parseToggleHaddock produces a parser for `toggle-haddock` command
parseToggleHaddock :: Mod CommandFields Cmd
parseToggleHaddock = command "toggle-haddock" parserInfo
  where
    parserInfo = info 
        (ToggleHaddock <$> pure haddockPrefix) 
        (progDesc "Toggle Haddock comment on/off over the current line.")

parseToggleBlock :: Mod CommandFields Cmd
parseToggleBlock  = command "toggle-block" parserInfo
  where
    parserInfo = info (ToggleBlock <$> pure (fst blockCommentPS)  <*> pure (snd blockCommentPS) <*> parseOptions) 
                      (progDesc "Toggle comment on/off over a range.")
-- |     parseCoords produces a parser for option: --coords=[x1, y1, x2, y2]
    parseCoords :: Parser Coords
    parseCoords =  option auto 
               (  long "coords"
               <> help "Coordinates of the starting and end points."
               <> metavar "[X1, Y1, X2, Y2]"
               )
    parseOptions = parseCoords

----------------------------------------
---- Interaction
----------------------------------------

-- | runCmd executes runs the Cmd parser, getting arguments from command line
-- display help and produce error.
runCmd :: IO Cmd
runCmd = customExecParser p parseCmdInfo
  where
    p = prefs showHelpOnEmpty
