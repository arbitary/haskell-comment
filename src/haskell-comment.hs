--------------------
-- Import
--------------------

import RIO
import qualified RIO.Text as Text
import Test.Hspec
import System.Environment

--------------------
-- Data definition
--------------------

-- Length represents the number of character used for commenting
type Length = Int

-- Symbol represents the string used for commenting
type Symbol = Text

-- Line represents a line of text 
type Line = Text

--------------------
-- Function
--------------------

-- isLineCommented consumes a line of text
-- ; it returns True if the line starts with "--"
-- after removing leading spaces.
isLineCommented :: Line -> Bool
isLineCommented = ("--" `Text.isPrefixOf`) . Text.strip

-- commentLine line consumes a line
-- ; produces a line with "-- " prepended to the beginning of line
commentLine :: Line -> Line
commentLine = ("-- " <>)

-- uncommentLine `line` consumes a line of text
-- it returns a new Line with any '-', ' ' in `line` before the 
-- the first alpha chracter removed.
uncommentLine :: Line -> Line
uncommentLine = Text.dropPrefix "-- "
-- uncommentLine = Text.stripStart . Text.dropWhile (pure (||) <*> (== '-') <*> (== ' '))

toggleLineComment :: Line -> Line
toggleLineComment = bool <$> commentLine <*> uncommentLine <*> isLineCommented  


-- TODO toggleBlockComment = undefined

addSection :: Length -> Symbol -> [Text] -> Text
addSection n symbol ts = Text.unlines $ [line] <> map prependLine ts <> [line]
  where
    prependLine :: Text -> Text
    prependLine = Text.append (Text.replicate 2 symbol <> " ")
    line        = Text.replicate n symbol

-- f consumes all input                            
-- ; it produces a new block comment with provided 
-- input strings embeded.                          
f :: String -> String
f = Text.unpack . addSection 20 "-" . Text.lines . Text.strip . Text.pack

-- processInput f consumes a String: s
-- ; it returns a new String by applying f to each line of s
processInput :: (Line -> Line) -> String -> String
processInput f = Text.unpack . Text.unlines . fmap f . Text.lines . Text.strip . Text.pack

--------------------
-- Interaction
--------------------

-- ! accept arguments
main :: IO ()
main = do 
  getArgs >>= \case 
    [subcommand] -> case subcommand of
                      "toggle-line" -> interact (processInput toggleLineComment) 
                      _             -> putStrLn $  "Incorrect sub command. Available sub commands: " 
                                                <> "{ toggle-line | toggle-block | toggle-section }"
    _            -> 
                      putStrLn $  "Incorrect format." <> " "  <> "Usage: " 
                               <> "haskell-comment { toggle-line | toggle-block | toggle-section }"
  
-- test
-- interact f


--------------------
-- Test
--------------------
test = hspec $ do
  describe "isLineCommented" $ do
    it "should work on commentted line with leading space:" $
      isLineCommented " -- function-" `shouldBe` True
    it "shouldn't work on lines with '--' mixed in the middile and end:" $
      isLineCommented " -function --" `shouldBe` False

  describe "commentLine" $ do
    it "should respect existing format:" $
      commentLine " -- function-" `shouldBe` "--  -- function-"

  describe "uncommentLine" $ do
    it "shouldn't remove nested commends and leading space:" $
      uncommentLine "--  --function-" `shouldBe` " --function-"

  describe "toggleLineComment" $ do
    it "should remove -- :" $
      toggleLineComment "--  --function-" `shouldBe` " --function-"
    it "shouldn't remove anything:" $
      toggleLineComment " --function-" `shouldBe` " --function-"
