module Type where
--------------------
-- Import
--------------------

import           RIO

--------------------
-- Data definition
--------------------

-- Comment prefix is represented as a piece of Text
type Prefix = Text

-- Comment suffix is represented as a piece of Text
type Suffix = Text

-- coordinates of the beginning and end points is represented
-- as a list of integers.
-- corrds => [x1, y1, x2, y2]
type Coords = [Int]

-- | A line of input is represented a line of text 
type Line = Text

-- | Block 
type Block = Text

-- A command consists one of:
data Cmd 
    = ToggleLine 
      { prefix :: Prefix }                  -- ^ Add or remove prefix of a line of text
    | ToggleHaddock 
      { prefix :: Prefix }               -- ^ Add or remove prefix of a line of text
    | ToggleBlock 
      { prefix :: Prefix
      , suffix :: Suffix
      , coords :: Coords
      }
    | AddSection 
      { prefix :: Prefix }
    deriving (Show)
