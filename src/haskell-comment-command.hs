--------------------
-- Import
--------------------

import           RIO
import qualified RIO.Text as Text
import qualified RIO.List as List
import           Test.Hspec
import           System.Environment

--------------------
-- Data
--------------------

haddockPrefix      = "-- | "
lineCommentPrefix  = "-- "
blockCommentPS = ("{-", "-}")

--------------------
-- Data definition
--------------------

data BlockComment = BlockComment
    { prefix :: Text
    , suffix :: Text
    , corrds :: Text
    }

-- | Symbol represents the Text used to build the section box
type Symbol = Text

-- | Block resprents a block of input and output
type Block = String

-- | Line represents a line of String 
type Line = Text

-- CommentPrefix represents prefix used to prepend
-- ; to a Text
type CommentPrefix = Text

type Coords = [(Int, Int)]

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
commentLine :: CommentPrefix -> Line -> Line
commentLine = (<>)

-- uncommentLine prefix line returns a new Line with '-- ' removed.
uncommentLine :: CommentPrefix -> Line -> Line
uncommentLine = Text.dropPrefix
{-TODO: should be able to uncomment line with spaces before '--" "  -}

-- | 'toogleLineComment prefix line' comments/uncomments current line
-- toggleLineComment removes comments if current line is a haddock comment or 
-- a line comment. Trying to remove a line commnet from a haddockLine will returns the line unchanged;
-- the same for trying to remove a haddock line from a lineCommented line
-- Since haddock comment is line comment itself. Removing line comment
-- from a haddock comment doesn't affact the current line.
toggleLineComment ::  CommentPrefix -> Line -> Line
toggleLineComment commentPrefix line 
  | isHaddockLine line, commentPrefix == haddockPrefix = uncommentLine commentPrefix line
  | isHaddockLine line, commentPrefix /= haddockPrefix = line
  | isLineCommented line                               = uncommentLine commentPrefix line
  | otherwise = commentLine commentPrefix line

commentBlock :: Coords -> [Line] -> [Line]
commentBlock coords  = \case
    []              -> [] -- ! TODO
    [line]          -> [insertAt (y2 + Text.length prefix) suffix . insertAt (y1 - 1)  prefix  $ line]  
    [line1, line2]  -> [insertAt (y1 - 1) prefix line1, insertAt y2 suffix line2]
    (line1 : lines) -> [insertAt (y1 - 1) prefix line1] ++ init lines ++ [insertAt y2 suffix (last lines)]
  where
    [(x1, y1), (x2, y2)] = coords
    prefix = fst blockCommentPS
    suffix = snd blockCommentPS

uncommentBlock :: Coords -> [Line] -> [Line]
uncommentBlock coords lines  
    | [line] <- lines 
    = [deleteAt (y2 - Text.length suffix - Text.length prefix) suffix . deleteAt (y1 - 1) prefix $ line]
    | (p,s) <- Text.splitAt (y1-1) . head $ lines
    , let first = p <> Text.dropPrefix prefix s
    , (p',s') <- Text.splitAt y2 . last $ lines
    , let last  = Text.dropSuffix suffix p' <> s'
    = [first] ++ torso lines ++ [last]
  where
    [(x1, y1), (x2, y2)] = coords
    prefix = fst blockCommentPS
    suffix = snd blockCommentPS

-- | isBlockComment coords string tests whether the start and end of block comment symbols
-- at the specified coords. 
isBlockCommented :: Coords -> [Line] -> Bool
isBlockCommented coords = pure (&&) <*> Text.isPrefixOf(fst blockCommentPS) . Text.drop (y1-1) . head
                          <*> Text.isSuffixOf(snd blockCommentPS) . Text.take y2 . last
  where
    [(x1, y1), (x2, y2)] = coords
    {-start = (1, y1)
    end   = (x2 - x1 + 1, y2)-}

-- "insertAt i x t" inserts x at index i
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

toggleBlockComment :: Coords -> [Line] -> [Line]
toggleBlockComment coords = pure bool <*> commentBlock coords <*> uncommentBlock coords <*> isBlockCommented coords
{-toggleBlockComment coords lines 
    | isBlockCommented coords lines
    = uncommentBlock coords lines
    | otherwise = commentBlock coords lines-}
    

addSection :: Int -> Symbol -> [Text] -> Text
addSection n symbol ts = Text.unlines $ [line] <> map prependLine ts <> [line]
  where
    prependLine :: Text -> Text
    prependLine = Text.append (Text.replicate 2 symbol <> " ")
    line        = Text.replicate n symbol 

-- | "inputToSection ss" turns ss into a section. e.g.:
inputToSection :: String -> String
inputToSection = Text.unpack . addSection 20 "-" . Text.lines . Text.stripEnd . Text.pack

-- processInput f consumes a String: s
-- ; it returns a new String by applying f to each line of s
processInput :: (Line -> Line) -> String -> String
processInput toggleLine = toBlock . fmap toggleLine . toLines

-- | "getCoords s, returns the corrds value from s.
-- "[[15, 1],[15, 2147483647]]" => [(15,1),(15,2147483647)]
getCoords :: [Char] -> [(Int, Int)]
getCoords l = do 
  [x, y] <- read (List.dropPrefix "--coords=" l) :: [[Int]]
  return (x, y)

-- | "toLines block" breaks a block of input into a list of lines
toLines :: Block  -> [Line]
toLines = Text.lines . Text.pack

-- | "toBlock" is the inverse of toLines.
toBlock :: [Line] -> Block
toBlock = Text.unpack . Text.unlines

--------------------
-- Interaction
--------------------

main :: IO ()
main =  
  getArgs >>= \case 
    [subcommand]
      | subcommand == "toggle-line"
      -> interact (toBlock . fmap (toggleLineComment lineCommentPrefix) . toLines)
      | subcommand == "toggle-haddock"
      -> interact (toBlock . fmap (toggleLineComment haddockPrefix) . toLines) 
      | subcommand == "toggle-section" 
      -> interact inputToSection
      | otherwise 
      ->  putStrLn $ "Incorrect sub command. available sub commands: " 
                <> "{ toggle-line | toggle-haddock | toggle-block | toggle-section }" 
    [subcommand, coords]
        | subcommand == "toggle-block"
        , Text.isPrefixOf "--coords=" . Text.strip. Text.pack $ coords
        -> interact (toBlock . (toggleBlockComment <$> const (getCoords coords) <*> toLines))
        | otherwise -> putStrLn $ "replace this with standard error :: " <> subcommand <> " " <> coords
    _                    -> putStrLn $  "Incorrect format." <> " "  <> "Usage: " 
                                     <> "hcc { toggle-line | toggle-haddock | toggle-block | toggle-section }"
