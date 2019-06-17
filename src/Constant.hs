module Constant where

import Type

haddockPrefix      = "-- | "  :: Prefix
lineCommentPrefix  = "-- "    :: Prefix
blockCommentPS = ("{-", "-}") :: (Prefix, Suffix)
sectionPrefix  = "-"         :: Prefix
