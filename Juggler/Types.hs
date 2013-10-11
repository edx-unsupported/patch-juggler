{-# LANGUAGE DeriveFunctor #-}
module Juggler.Types where

import Data.Text (Text)
import Data.Number.PartialOrd

data Padded a b = Padding a | Content b
    deriving (Functor, Show)

type PaddedList a b = [Padded a b]
pad = map Content

data Range = Range {
    r_start :: Int,
    r_length :: Int
} deriving Show

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
    h_src :: Range,
    h_dst :: Range,
    h_output :: [Text],
    h_gen :: Int
} deriving (Show)

r_end r = r_start r + r_length r

overlap a b = (r_end a) >= (r_start b) && (r_end b) >= (r_start a)

instance PartialOrd Hunk where
    cmp left right
        | (h_gen left) < (h_gen right) && overlap (h_dst left) (h_src right) = Just LT
        | (h_gen left) > (h_gen right) && overlap (h_src left) (h_dst right) = Just GT
        | otherwise = Nothing

data FileHunks = FileHunks {
    fh_name :: Text,
    fh_msg :: Text,
    fh_contents :: [Hunk]
}

data FileCommit = FileCommit {
    fc_name :: Text, -- File Name
    fc_msg :: Text, -- Commit Message
    fc_contents :: PaddedList PaddingLine Line
}

type FileGrid = [FileCommit]
type Dirty = Bool
data Line = Original Text
          | Dirty Text
          | Added Int Text
          | Elision

data PaddingLine = Deleted Int
                 | Filler
