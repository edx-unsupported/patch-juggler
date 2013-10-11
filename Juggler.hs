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

deltaFiles d = S.fromList [fd_source d, fd_dest d]
intersects x = (> 0) . S.size . S.intersection x
onlyFiles fs c = c {c_deltas = filter ((intersects fs) . deltaFiles) $ c_deltas c}
fillCommit fs c = c {c_deltas = c_deltas c ++ [FileDelta f f [] | f <- S.toList newFiles]}
    where newFiles = S.difference fs (S.unions $ map deltaFiles $ c_deltas c)

fileChains commits = [map (fillCommit fileSet . onlyFiles fileSet) commits | fileSet <- renameSets]
    where
        deltas = concatMap c_deltas commits
        allFiles = concatMap (\d -> [fd_source d, fd_dest d]) deltas
        renameSets = clusterSets $ map deltaFiles deltas

-- Apply hunk to the topmost file in the filetable
applyHunk :: FileGrid -> Hunk -> FileGrid
applyHunk (tip:rest) hunk
    | srcSize > dstSize = (insertContent Padding (r_end $ h_dst hunk) (replicate padding $ Deleted (h_gen hunk)) $ replaceContentRange Content repl contents tip):rest'
    | otherwise = (replaceContentRange Content repl contents tip):(map (insertRaw (contentToRaw tip $ r_end repl) (replicate padding $ Padding Filler)) rest')
    where
        rest' = map (mapRawRange (src {r_start = contentToRaw (head rest) $ r_start src}) (fmap markDirty)) rest
        rangeSize = uncurry (-) . swap
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
addHunks :: FileGrid -> [Hunk] -> FileGrid
addHunks table hunks = L.foldl' applyHunk ((map copyLine $ head table):table) hunks

fillTable baseline hunks = L.foldl' addHunks [pad $ map Original $ baseline] hunks

fillTable' commits = do
    let sha = T.unpack $ c_sha $ head commits
        file = T.unpack $ fd_source $ head $ c_deltas $ head commits
    sourceContents <- sh $ "git show " ++ sha ++ "^:" ++ file
    return $ fillTable (T.lines sourceContents) $ map fd_hunks $ concatMap c_deltas commits

main = do
    args <- getArgs
    gitLog <- sh $ L.intercalate " " ("git log --reverse -p -U0 --pretty='format:%H%n----%n%B%n----%n'":args)
    let commits = fromJust $ maybeResult $ newlineTerminate $ parse (orderedCommits <* endOfInput) gitLog
        chains = fileChains $ commits
        maxGen = length commits
    tables <- mapM fillTable' chains
    putStr $ renderHtml $ page [show chains, T.unpack gitLog] maxGen tables

