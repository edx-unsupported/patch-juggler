{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Juggler.Types where

import Prelude hiding (length)
import Control.Lens
import Data.Text (Text)
import Data.Number.PartialOrd

data Padded a b = Padding a | Content b
    deriving (Functor, Show)

type PaddedList a b = [Padded a b]
pad = map Content

type Dirty = Bool
data Line = Original Text
          | Dirty Text
          | Added Int Text
          | Elision

data PaddingLine = Deleted Int
                 | Filler

data Range = Range {
    _rangeStart :: Int,
    _rangeLength :: Int
} deriving Show

$(makeFields ''Range)

data Hunk = Hunk {
    _hunkSrc :: Range,
    _hunkDst :: Range,
    _hunkOutput :: [Text],
    _hunkGen :: Int
} deriving (Show)

$(makeFields ''Hunk)

data FileDelta = FileDelta {
    _filedeltaSource :: Text,
    _filedeltaDest :: Text,
    _filedeltaHunks :: [Hunk]
} deriving (Show)

$(makeFields ''FileDelta)

data Commit = Commit {
    _commitSha :: Text,
    _commitMsg :: Text,
    _commitDeltas :: [FileDelta]
} deriving (Show)

$(makeFields ''Commit)

data FileHunks = FileHunks {
    _filehunksName :: Text,
    _filehunksMsg :: Text,
    _filehunksContents :: [Hunk]
}

$(makeFields ''FileHunks)

data FileCommit = FileCommit {
    _filecommitName :: Text, -- File Name
    _filecommitMsg :: Text, -- Commit Message
    _filecommitContents :: PaddedList PaddingLine Line
}

$(makeFields ''FileCommit)

type FileGrid = [FileCommit]

end :: (Num a, HasLength s a, HasStart s a) => Lens' s a
end = lens getter setter
    where getter r = r ^. start + r ^. length
          setter r e = r & length .~ (e - r ^. start)


overlap :: Range -> Range -> Bool
overlap a b = (a ^. end) >= (b ^. start) && (b ^. end) >= (a ^. start)

instance PartialOrd Hunk where
    cmp left right
        | (left ^. gen) < (right ^. gen) && overlap (left ^. dst) (right ^. src) = Just LT
        | (left ^. gen) > (right ^. gen) && overlap (left ^. src) (right ^. dst) = Just GT
        | otherwise = Nothing
