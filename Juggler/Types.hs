module Juggler.Types where

import Data.Text (Text)
import Data.Number.PartialOrd

data Commit = Commit {
    c_sha :: Text,
    c_msg :: Text,
    c_deltas :: [FileDelta]
} deriving (Show)

data FileDelta = FileDelta {
    fd_source :: Text,
    fd_dest :: Text,
    fd_hunks :: [Hunk]
} deriving (Show)

data Hunk = Hunk {
    h_src :: (Int, Int),
    h_dst :: (Int, Int),
    h_output :: [Text],
    h_gen :: Int
} deriving (Show)

overlap (a, b) (x, y) = or [
    a <= x && x <= b,
    a <= y && y <= b,
    x <= a && a <= y,
    x <= b && b <= y
    ]

instance PartialOrd Hunk where
    cmp left right
        | (h_gen left) < (h_gen right) && overlap (h_dst left) (h_src right) = Just LT
        | (h_gen left) > (h_gen right) && overlap (h_src left) (h_dst right) = Just GT
        | otherwise = Nothing
