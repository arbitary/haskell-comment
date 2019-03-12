--------------------
-- Import
--------------------

import           RIO
import qualified RIO.Text as Text
import           Test.Hspec
import           System.Environment

--------------------
-- Data
--------------------

haddockPrefix     = "-- | "
lineCommentPrefix = "-- "

--------------------
-- Data definition
--------------------

-- Length represents the number of character used for commenting
type Length = Int

-- Symbol represents the string used for commenting
type Symbol = Text

-- Line represents a line of String 
type Line = Text

-- CommentPrefix represents prefix used to prepend
-- ; to a Text
type CommentPrefix = Text


--------------------
-- Function
--------------------

-- isLineCommented consumes a line of text
-- ; it returns True if the line starts with "--"
-- after removing leading spaces.
isLineCommented :: Line -> Bool
isLineCommented = ("--" `Text.isPrefixOf`) . Text.strip 

isHaddockLine :: Line -> Bool
isHaddockLine = ("-- |" `Text.isPrefixOf`) . Text.strip
  
-- commentLine prefix line retuturns a Line with "-- " prepended to `line`
commentLine :: CommentPrefix -> Line -> Line
commentLine = (<>)

-- uncommentLine prefix line returns a new Line with '-- ' removed.
uncommentLine :: CommentPrefix -> Line -> Line
uncommentLine = Text.dropPrefix
-- TODO: should be able to uncomment line with spaces before '--" "   -- function"

-- | 'toogleLineComment prefix line' comments/uncomments current line
-- toggleLineComment removes comments if current line is a haddock comment or 
-- a line comment. Since haddock comment is line comment itself. Removing line comment
-- from a haddock comment doesn't affact the current line.
toggleLineComment ::  CommentPrefix -> Line -> Line
toggleLineComment commentPrefix line 
  | isHaddockLine line, commentPrefix == haddockPrefix = uncommentLine commentPrefix line
  | isHaddockLine line, commentPrefix /= haddockPrefix = line
  | isLineCommented line                               = uncommentLine commentPrefix line
  | otherwise = commentLine commentPrefix line


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
f = Text.unpack . addSection 20 "-" . Text.lines . Text.stripEnd . Text.pack

-- processInput f consumes a String: s
-- ; it returns a new String by applying f to each line of s
-- | processInput :: (Line -> Line) -> String -> String
processInput toggleFunc = Text.unpack . Text.unlines . fmap toggleFunc 
                        . Text.lines .  Text.pack

--------------------
-- Interaction
--------------------

main :: IO ()
main = do 
  getArgs >>= \case 
    [subcommand] -> case subcommand of
                      "toggle-line"    -> interact (processInput (toggleLineComment lineCommentPrefix)) 
                      "toggle-haddock" -> interact (processInput (toggleLineComment haddockPrefix)) 
                      _                -> putStrLn $  "Incorrect sub command. Available sub commands: " 
                                                <> "{ toggle-line | toggle-haddock | toggle-block | toggle-section }"
    _            -> 
                      putStrLn $  "Incorrect format." <> " "  <> "Usage: " 
                               <> "haskell-comment { toggle-line | toggle-block | toggle-section }"
  
-- test
-- interact f


--------------------
-- Test
--------------------
-- test = hspec $ do
--   describe "isLineCommented" $ do
--     it "should work on commentted line with leading space:" $
--       isLineCommented " -- function-" `shouldBe` True
--     it "shouldn't work on lines with '--' mixed in the middile and end:" $
--       isLineCommented " -function --" `shouldBe` False
-- 
--   describe "commentLine" $ do
--     it "should respect existing format:" $
--       commentLine "-- " " -- function-" `shouldBe` "--  -- function-"
-- 
--   describe "uncommentLine" $ do
--     it "shouldn't remove nested commends and leading space:" $
--       uncommentLine "-- " "--  --function-" `shouldBe` " --function-"
-- 
--   describe "toggleLineComment" $ do
--     it "should remove -- :" $
--       toggleLineComment "-- " "--  --function-" `shouldBe` " --function-"
--     it "shouldn't remove anything:" $
--       toggleLineComment "-- " " --function-" `shouldBe` " --function-"
