module Main where

--------------------
-- Import
--------------------

import           RIO
import qualified RIO.Text as Text
import qualified Data.Text.IO as TIO
import           Constant
import           Type
import           Command

--------------------
-- Data
--------------------

--------------------
-- Data definition
--------------------

--------------------
-- Function
--------------------

-- | "isLineCommented line" returns True
-- if the line starts with "--" or with only space as its prefix
-- e.g. "  --" 
isLineCommented :: Line -> Bool
isLineCommented = ("--" `Text.isPrefixOf`) . Text.strip 

isHaddockLine :: Line -> Bool
isHaddockLine = ("-- |" `Text.isPrefixOf`) . Text.strip
  
-- | "commentLine prefix line" retuturns a Line with "-- " prepended to `line`
commentLine :: Prefix -> Line -> Line
commentLine = (<>)

-- uncommentLine prefix line returns a new Line with '-- ' removed.
uncommentLine :: Prefix -> Line -> Line
uncommentLine = Text.dropPrefix
{-TODO: should be able to uncomment line with spaces before '--" "  -}


-- | 'toogleLineComment prefix line' comments/uncomments current line
-- toggleLineComment removes comments if current line is a haddock comment or 
-- a line comment. Trying to remove a line commnet from a haddockLine will returns the line unchanged;
-- the same for trying to remove a haddock line from a lineCommented line
-- Since haddock comment is line comment itself. Removing line comment
-- from a haddock comment doesn't affact the current line.
-- toggleLineComment ::  CommentPrefix -> Line -> Line
toggleLineComment :: Cmd -> Line -> Line 
toggleLineComment (prefix -> prefix) line 
    | isHaddockLine line, prefix == haddockPrefix = uncommentLine prefix line
    | isHaddockLine line, prefix /= haddockPrefix = line
    | isLineCommented line                        = uncommentLine prefix line
    | otherwise = commentLine prefix line


commentBlock :: Cmd -> [Line] -> [Line]
commentBlock cmd    = \case
    []              -> [] -- ! TODO
    [line]          -> [insertAt (y2 + Text.length prefix) suffix . insertAt (y1 - 1)  prefix  $ line]  
    -- [line1, line2]  -> [insertAt (y1 - 1) prefix line1, insertAt y2 suffix line2]
    (line1 : lines) -> [insertAt (y1 - 1) prefix line1] ++ init lines ++ [insertAt y2 suffix (last lines)]
  where
    ToggleBlock prefix suffix [x1, y1, x2, y2] = cmd

uncommentBlock :: Cmd -> [Line] -> [Line]
uncommentBlock cmd lines  
    | [line] <- lines 
    = [deleteAt (y2 - Text.length suffix - Text.length prefix) suffix 
      . deleteAt (y1 - 1) prefix $ line]
    | (p,s) <- Text.splitAt (y1-1) . head $ lines
    , let first = p <> Text.dropPrefix prefix s
    , (p',s') <- Text.splitAt y2 . last $ lines
    , let last  = Text.dropSuffix suffix p' <> s'
    = [first] ++ torso lines ++ [last]
  where
    ToggleBlock prefix suffix [x1, y1, x2, y2] = cmd

-- | isBlockComment coords string tests whether the start and end of block comment symbols
-- at the specified coords. 
isBlockCommented :: Cmd -> [Line] -> Bool
isBlockCommented cmd = pure (&&) 
    <*> Text.isPrefixOf prefix . Text.drop (y1-1) . head
    <*> Text.isSuffixOf(snd blockCommentPS) . Text.take y2 . last
  where
    ToggleBlock prefix suffix [x1, y1, x2, y2] = cmd

-- "insertAt i x t" inserts Text: x into Text: t at index i
insertAt :: Int -> Text -> Text -> Text
insertAt i x t = Text.take i t <> x <> Text.drop i t

-- | "deleteAt i junk t" returns t with junk removed.
deleteAt :: Int -> Text -> Text -> Text
deleteAt i junk t = pure (<>) <*> fst <*> Text.dropPrefix junk . snd $ Text.splitAt i t

-- | "torso xs" returns xs without its first and last element.
-- it returns the torso part if xs contains at least 3 elements.
-- [1, 2, 3] => [2]; [], [x, y], [x] => []
torso :: [a] -> [a]
torso xs = bool [] (init . tail $ xs) (length xs >= 3)

toggleBlockComment :: Cmd -> [Line] -> [Line]
toggleBlockComment cmd = pure bool <*> commentBlock cmd <*> uncommentBlock cmd <*> isBlockCommented cmd
    

addSection :: Int -> Prefix -> [Line] -> [Line]
addSection n prefix lines =  [line] <> map prependLine lines <> [line]
  where
--  prependline appends "-- " to the existing line of text
    prependLine :: Line -> Line
    prependLine = Text.append (Text.replicate 2 prefix <> " ") 
--     line produces "--------------------"
    line        = Text.replicate n prefix   


-- | "toLines block" breaks a block of input into a list of lines
toLines :: Block  -> [Line]
toLines = Text.lines 

-- | "toBlock" is the inverse of toLines.
toBlock :: [Line] -> Block
toBlock = Text.unlines

--------------------
-- Interaction
--------------------

main :: IO ()
main = runCmd >>= \case 
    cmd@ToggleBlock {}        -> 
        TIO.interact (toBlock . toggleBlockComment cmd . toLines)
    cmd@(AddSection _)        -> 
        TIO.interact (toBlock . addSection 20 sectionPrefix . toLines {-. Text.stripEnd-})
    cmd                       -> 
        TIO.interact (toBlock . fmap (toggleLineComment cmd) . toLines)
    
     
