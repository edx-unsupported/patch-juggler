{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding ((.), id, length)
import Control.Lens
import Control.Category
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

clusterSets :: (Ord a) => [S.Set a] -> [S.Set a]
clusterSets [] = []
clusterSets (x:xs) =
    if (L.length withX) == 0
        then x:clusterSets withoutX
        else clusterSets $ (S.unions $ x:withX):withoutX
    where (withX, withoutX) = L.partition (intersects x) xs

-- The set of files affected by a FileDelta
deltaFiles :: FileDelta -> S.Set T.Text
deltaFiles d = S.fromList [d ^. source, d ^. dest]

intersects x = (> 0) . S.size . S.intersection x

onlyFiles :: S.Set T.Text -> Commit -> Commit
onlyFiles fs = over deltas $ filter ((intersects fs) . deltaFiles)

fillCommit fs c = over deltas (++ [FileDelta f f [] | f <- S.toList newFiles]) c
    where newFiles = S.difference fs (S.unions $ map deltaFiles $ c ^. deltas)

fileChains :: [Commit] -> [[Commit]]
fileChains commits = [map (fillCommit fileSet . onlyFiles fileSet) commits | fileSet <- renameSets]
    where
        allDeltas = concatMap (view deltas) commits
        allFiles = concatMap (\d -> [d ^. source, d ^. dest]) allDeltas
        renameSets = clusterSets $ map deltaFiles allDeltas

rangeToRaw = over start . contentToRaw
markContentRangeDirty range = over contents update
    where update contents = mapRawRange (rangeToRaw contents range) (fmap markDirty) contents

appendRawFiller pos amount = over contents $ insertRaw pos (replicate amount $ Padding Filler)

-- Apply hunk to the topmost file in the filetable
applyHunk :: FileGrid -> Hunk -> FileGrid
applyHunk (tip:rest) hunk = (set contents tipContents' tip):rest'
    where
        rest' = if srcSize > dstSize
            then restDirty
            else map (appendRawFiller (contentToRaw tipContents $ repl ^. end) padding) restDirty
        tipContents' = if srcSize > dstSize
            then (insertContent Padding (hunk ^. dst . end) (replicate padding $ Deleted (hunk ^. gen)) $ replaceContentRange Content repl contents' tipContents)
            else (replaceContentRange Content repl contents' tipContents)
        tipContents = tip ^. contents
        restDirty = map (markContentRangeDirty $ hunk ^. src) rest
        repl = set length (hunk ^. src . length) $ hunk ^. dst
        srcSize = hunk ^. src . length
        dstSize = hunk ^. dst . length
        contents' = map (Added (hunk ^. gen)) $ hunk ^. output
        padding = abs $ srcSize - dstSize

markDirty (Original t) = Dirty t
markDirty x = x

copyLine (Content (Added gen t)) = Content (Dirty t)
copyLine (Padding (Deleted gen)) = Padding Filler
copyLine x = x

-- Add all of the hunks to the file table as a new column
addHunks :: FileGrid -> FileHunks -> FileGrid
addHunks table hunks = L.foldl' applyHunk (tip:table) (hunks ^. contents)
    where tip = FileCommit (hunks ^. name) (hunks ^. msg) (map copyLine $ view contents $ head table)

fillTable :: T.Text -> [T.Text] -> [FileHunks] -> FileGrid
fillTable filename baseline hunks = L.foldl' addHunks [FileCommit filename "Original" $ pad $ map Original $ baseline] hunks

fillTable' commits = do
    let sourceSha = view sha $ head commits
        sourceFile = view source $ head $ view deltas $ head commits
        fileHunks commit = FileHunks (view dest $ head $ commit ^. deltas) (commit ^. msg) (concatMap (view hunks) $ commit ^. deltas)
    sourceContents <- sh $ "git show " ++ (T.unpack sourceSha) ++ "^:" ++ T.unpack sourceFile
    return $ fillTable sourceFile (T.lines sourceContents) $ map fileHunks commits

main = do
    args <- getArgs
    gitLog <- sh $ L.intercalate " " ("git log --reverse -p -U0 --pretty='format:%H%n----%n%B%n----%n'":args)
    let commits = fromJust $ maybeResult $ newlineTerminate $ parse (orderedCommits <* endOfInput) gitLog
        chains = fileChains $ commits
        maxGen = L.length commits
    tables <- mapM fillTable' chains
    putStr $ renderHtml $ page [show chains, T.unpack gitLog] maxGen tables
