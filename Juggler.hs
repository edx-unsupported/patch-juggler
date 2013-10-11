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
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html.Renderer.String
import Debug.Trace
import Data.Monoid
import Numeric
import Data.Function

import Juggler.PaddedList
import Juggler.Parse
import Juggler.Types

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

traceVal x = traceShow x x

-- Apply hunk to the topmost file in the filetable
applyHunk :: FileGrid -> Hunk -> FileGrid
applyHunk (tip:rest) hunk
    | srcSize > dstSize = (insertContentPadding (r_end $ h_dst hunk) padding $ replaceContentRange repl contents tip):rest'
    | otherwise = (replaceContentRange repl contents tip):(map (insertRawPadding (r_end src) padding) rest')
    where
        rest' = map (mapRawRange src (fmap makeDirty)) rest
        rangeSize = uncurry (-) . swap
        src = h_src hunk
        repl = (h_dst hunk) {r_length = r_length src}
        srcSize = r_length $ h_src hunk
        dstSize = r_length $ h_dst hunk
        contents = map (HunkLn (h_gen hunk)) $ h_output hunk
        padding = abs $ srcSize - dstSize

makeDirty (SourceLn False t) = SourceLn True t
makeDirty x = x

copyLine (HunkLn gen t) = SourceLn True t
copyLine x = x

-- Add all of the hunks to the file table as a new column
addHunks :: FileGrid -> [Hunk] -> FileGrid
addHunks table hunks = L.foldl' applyHunk ((map (fmap copyLine) $ head table):table) hunks

fillTable baseline hunks = L.foldl' addHunks [pad $ map (SourceLn False) $ baseline] hunks

fillTable' commits = do
    let sha = T.unpack $ c_sha $ head commits
        file = T.unpack $ fd_source $ head $ c_deltas $ head commits
    sourceContents <- sh $ "git show " ++ sha ++ "^:" ++ file
    return $ fillTable (T.lines sourceContents) $ map fd_hunks $ concatMap c_deltas commits

nbsp :: H.Html
nbsp = H.toHtml ("\160" :: String)

formatTextLine line = H.pre $ do
    H.toHtml (T.stripEnd line)
    nbsp

formatLine :: Padded Line -> H.Html
formatLine Padding = H.pre $ nbsp
formatLine (Content ElisionLn) = H.div ! HA.class_ "elision" $ H.toHtml ("\8942" :: String)
formatLine (Content (SourceLn _ t)) = formatTextLine t
formatLine (Content (HunkLn gen t)) = formatTextLine t ! HA.class_ (mappend "gen-" (H.toValue gen))

elideLines lines = lines'
    where
        len = length lines
        lines' = if len > 3
            then (L.take 2 lines) ++ [Content ElisionLn] ++ L.drop (len - 2) lines
            else lines

formatLineGroup :: PaddedList Line -> H.Html
formatLineGroup lines@(Padding:rest) = H.div ! HA.class_ "insert-line" $ mapM_ formatLine lines
formatLineGroup lines@(Content (SourceLn True _):rest) = H.div ! HA.class_ "source-line" $ mapM_ formatLine lines
formatLineGroup lines@(Content (SourceLn False _):rest) = H.div ! HA.class_ "source-line" $ mapM_ formatLine $ elideLines lines
formatLineGroup lines@(Content (HunkLn _ _):rest) = H.div ! HA.class_ "hunk-line" $ mapM_ formatLine lines

formatTableEntry :: PaddedList Line -> H.Html
formatTableEntry = H.td . mapM_ formatLineGroup . L.groupBy ((==) `on` lineType)

formatTable :: FileGrid -> H.Html
formatTable table = H.tr $ mapM_ formatTableEntry (reverse table)

formatTables :: [FileGrid] -> H.Html
formatTables = H.table . mapM_ formatTable

lineType Padding = 0
lineType (Content (SourceLn True _)) = 1
lineType (Content (SourceLn False _)) = 2
lineType (Content (HunkLn _ _)) = 3

main = do
    args <- getArgs
    gitLog <- sh $ L.intercalate " " ("git log --reverse -p -U0 --pretty='format:%H%n----%n%B%n----%n'":args)
    let chains = fileChains $ fromJust $ maybeResult $ newlineTerminate $ parse (orderedCommits <* endOfInput) gitLog
    tables <- mapM fillTable' chains
    let
        styles = [
            "td {vertial-align: top;}",
            "* {margin: 0;}",
            ".insert-line {background-color: lightgray;}",
            ".elision {text-align: center;}"
            ] ++ [
            ".hunk-line .gen-" ++ show gen ++ "{background-color: #f" ++ showHex gen "f;}"
            | gen <- [0..15]
            ]
    putStr $ "<style>" ++ unlines styles ++ "</style>"
    putStr "<meta http-equiv='Content-Type' content='text/html; charset=utf-8'>"
    putStr $ "<!--" ++ show chains ++ "-->"
    putStr $ renderHtml $ formatTables tables
