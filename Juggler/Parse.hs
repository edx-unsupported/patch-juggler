{-# LANGUAGE OverloadedStrings #-}

module Juggler.Parse where


import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import qualified Data.Text as T
import Juggler.Types

separator = (string "----" >> endOfLine) <?> "separator"

line = do
    ln <- takeTill isEndOfLine
    endOfLine
    return ln

setHunkGeneration gen hunk = hunk {h_gen = gen}
setDeltaGeneration gen delta = delta {fd_hunks = map (setHunkGeneration gen) $ fd_hunks delta}
setCommitGeneration gen commit = commit {c_deltas = map (setDeltaGeneration gen) $ c_deltas commit}

orderedCommits = do
    commits <- many commit
    return $ zipWith (setCommitGeneration) [1..] commits

commit = do
    sha <- line <?> "commit sha"
    separator <?> "after sha"
    msg <- manyTill line separator <?> "commit msg"
    endOfLine
    deltas <- option [] (many1 fileDelta <* endOfLine) <?> "commit file deltas"
    return $ Commit sha (T.unlines msg) deltas

fileDelta = do
    string "diff --git " <?> "file delta prefix"
    source <- ("a/" .*> takeTill isSpace) <?> "file delta source"
    space
    dest <- ("b/" .*> line) <?> "file delta dest"
    option "" (string "new file mode" >> line) <?> "file delta new file"
    option "" (string "index" >> line) <?> "file delta index" -- index $hash..$hash perms
    option "" (string "---" >> line) <?> "file delta source header" -- --- source
    option "" (string "+++" >> line) <?> "file delta dest header" -- +++ dest
    hunks <- option [] (many1 hunk) <?> "file delta hunks"
    return $ FileDelta source dest hunks

hunk = do
    string "@@ "
    sourceStart <- "-" .*> decimal
    sourceEnd <- option sourceStart ("," .*> decimal)
    string " "
    destStart <- "+" .*> decimal
    destEnd <- option destStart ("," .*> decimal)
    string " @@"
    line
    skipMany ("-" >> line)
    output <- option [] $ many1 ("+" .*> line)
    return $ Hunk (sourceStart, sourceEnd) (destStart, destEnd) output 0

newlineTerminate = flip feed "" . flip feed "\n"