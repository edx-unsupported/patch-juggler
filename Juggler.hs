{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.List as L
import qualified Data.Text.IO as TIO
import System.Environment
import System.IO
import System.Process
import qualified Data.Set as S
import Data.Maybe
import Data.Tuple
import qualified Data.Text as T
import Text.Blaze.Html.Renderer.String

import Juggler.PaddedList
import Juggler.Parse
import Juggler.Types
import Juggler.Html

sh command = do
    -- putStrLn command
    (inp, out, err, pid) <- runInteractiveCommand $ command
    hClose inp
    TIO.hGetContents out

clusterSets [] = []
clusterSets (x:xs) =
    if (length withX) == 0
        then x:clusterSets withoutX
        else clusterSets $ (S.unions $ x:withX):withoutX
    where (withX, withoutX) = L.partition (intersects x) xs

-- The set of files affected by a FileDelta
deltaFiles :: FileDelta -> S.Set T.Text
deltaFiles d = S.fromList [fd_source d, fd_dest d]

intersects x = (> 0) . S.size . S.intersection x

onlyFiles :: S.Set T.Text -> Commit -> Commit
onlyFiles fs c = c {c_deltas = filter ((intersects fs) . deltaFiles) $ c_deltas c}

fillCommit fs c = c {c_deltas = c_deltas c ++ [FileDelta f f [] | f <- S.toList newFiles]}
    where newFiles = S.difference fs (S.unions $ map deltaFiles $ c_deltas c)

fileChains :: [Commit] -> [[Commit]]
fileChains commits = [map (fillCommit fileSet . onlyFiles fileSet) commits | fileSet <- renameSets]
    where
        deltas = concatMap c_deltas commits
        allFiles = concatMap (\d -> [fd_source d, fd_dest d]) deltas
        renameSets = clusterSets $ map deltaFiles deltas

rangeToRaw pl range = range {r_start = contentToRaw pl $ r_start range}
markContentRangeDirty range fc = fc {fc_contents = contents'}
    where contents = fc_contents fc
          contents' = mapRawRange (rangeToRaw contents range) (fmap markDirty) contents

appendRawFiller pos amount fc = fc {fc_contents = contents'}
    where contents = fc_contents fc
          contents' = insertRaw pos (replicate amount $ Padding Filler) contents

-- Apply hunk to the topmost file in the filetable
applyHunk :: FileGrid -> Hunk -> FileGrid
applyHunk (tip:rest) hunk = tip {fc_contents = tipContents'}:rest'
    where
        rest' = if srcSize > dstSize
            then restDirty
            else map (appendRawFiller (contentToRaw tipContents $ r_end repl) padding) restDirty
        tipContents' = if srcSize > dstSize
            then (insertContent Padding (r_end $ h_dst hunk) (replicate padding $ Deleted (h_gen hunk)) $ replaceContentRange Content repl contents tipContents)
            else (replaceContentRange Content repl contents tipContents)
        tipContents = fc_contents tip
        restDirty = map (markContentRangeDirty src) rest
        src = h_src hunk
        repl = (h_dst hunk) {r_length = r_length src}
        srcSize = r_length $ h_src hunk
        dstSize = r_length $ h_dst hunk
        contents = map (Added (h_gen hunk)) $ h_output hunk
        padding = abs $ srcSize - dstSize

markDirty (Original t) = Dirty t
markDirty x = x

copyLine (Content (Added gen t)) = Content (Dirty t)
copyLine (Padding (Deleted gen)) = Padding Filler
copyLine x = x

-- Add all of the hunks to the file table as a new column
addHunks :: FileGrid -> FileHunks -> FileGrid
addHunks table hunks = L.foldl' applyHunk (tip:table) (fh_contents hunks)
    where tip = FileCommit (fh_name hunks) (fh_msg hunks) (map copyLine $ fc_contents $ head table)

fillTable :: T.Text -> [T.Text] -> [FileHunks] -> FileGrid
fillTable filename baseline hunks = L.foldl' addHunks [FileCommit filename "Source" $ pad $ map Original $ baseline] hunks

fillTable' commits = do
    let sha = c_sha $ head commits
        sourceFile = fd_source $ head $ c_deltas $ head commits
        fileHunks commit = FileHunks (fd_dest $ head $ c_deltas commit) (c_msg commit) (concatMap fd_hunks $ c_deltas commit)
    sourceContents <- sh $ "git show " ++ (T.unpack sha) ++ "^:" ++ T.unpack sourceFile
    return $ fillTable sourceFile (T.lines sourceContents) $ map fileHunks commits

main = do
    args <- getArgs
    gitLog <- sh $ L.intercalate " " ("git log --reverse -p -U0 --pretty='format:%H%n----%n%B%n----%n'":args)
    let commits = fromJust $ maybeResult $ newlineTerminate $ parse (orderedCommits <* endOfInput) gitLog
        chains = fileChains $ commits
        maxGen = length commits
    tables <- mapM fillTable' chains
    putStr $ renderHtml $ page [show chains, T.unpack gitLog] maxGen tables

