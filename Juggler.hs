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
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Ratio

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

nbsp :: H.Html
nbsp = H.toHtml ("\160" :: String)

formatTextLine line = H.pre $ do
    H.toHtml (T.stripEnd line)
    nbsp

emptyLine = H.pre $ nbsp

formatLine :: Padded PaddingLine Line -> H.Html
formatLine (Padding Filler) = emptyLine
formatLine (Padding (Deleted gen)) = emptyLine ! HA.class_ (mappend "gen-" (H.toValue gen))
formatLine (Content Elision) = H.div ! HA.class_ "elision" $ H.toHtml ("\8942" :: String)
formatLine (Content (Original t)) = formatTextLine t
formatLine (Content (Dirty t)) = formatTextLine t
formatLine (Content (Added gen t)) = formatTextLine t ! HA.class_ (mappend "gen-" (H.toValue gen))

elideLines lines = lines'
    where
        len = length lines
        lines' = if len > 3
            then (L.take 2 lines) ++ [Content Elision] ++ L.drop (len - 2) lines
            else lines

formatLineGroup :: PaddedList PaddingLine Line -> H.Html
formatLineGroup lines@(Padding Filler:rest) = H.div ! HA.class_ "insert-line" $ mapM_ formatLine lines
formatLineGroup lines@(Padding (Deleted _):rest) = H.div ! HA.class_ "del-line" $ mapM_ formatLine lines
formatLineGroup lines@(Content (Dirty _):rest) = H.div ! HA.class_ "source-line dirty" $ mapM_ formatLine lines
formatLineGroup lines@(Content (Original _):rest) = H.div ! HA.class_ "source-line original" $ mapM_ formatLine $ elideLines lines
formatLineGroup lines@(Content (Added _ _):rest) = H.div ! HA.class_ "add-line" $ mapM_ formatLine lines

formatTableEntry :: PaddedList PaddingLine Line -> H.Html
formatTableEntry = H.td . mapM_ formatLineGroup . L.groupBy ((==) `on` lineType)

formatTable :: FileGrid -> H.Html
formatTable table = H.tr $ mapM_ formatTableEntry (reverse table)

formatTables :: [FileGrid] -> H.Html
formatTables = H.table . mapM_ formatTable

lineType (Padding Filler) = 0
lineType (Content (Dirty _)) = 1
lineType (Content (Original _)) = 2
lineType (Content (Added _ _)) = 3
lineType (Padding (Deleted _)) = 4

addColor max gen = hsv 112 0.2 (0.4 + (gen % max) * 0.5)
delColor max gen = hsv 0 0.5 (0.25 + (gen % max) * 0.7)

toCss r g b = (hex r) . (hex g) . (hex b)
    where hex = showHex . round . (256 *)

main = do
    args <- getArgs
    gitLog <- sh $ L.intercalate " " ("git log --reverse -p -U0 --pretty='format:%H%n----%n%B%n----%n'":args)
    let commits = fromJust $ maybeResult $ newlineTerminate $ parse (orderedCommits <* endOfInput) gitLog
        chains = fileChains $ commits
        maxGen = length commits
    tables <- mapM fillTable' chains
    let
        styles = [
            "td {vertical-align: top;}",
            "* {margin: 0;}",
            ".insert-line {background-color: lightgray;}",
            ".elision {text-align: center;}",
            ".source-line.original {color: darkgray;}"
            ] ++ [
            ".add-line .gen-" ++ show gen ++ "{background-color: #" ++ (uncurryRGB toCss $ addColor maxGen gen) ";}"
            | gen <- [0..maxGen]
            ] ++ [
            ".del-line .gen-" ++ show gen ++ "{background-color: #" ++ (uncurryRGB toCss $ delColor maxGen gen) ";}"
            | gen <- [0..maxGen]
            ]
    putStr $ "<style>" ++ unlines styles ++ "</style>"
    putStr "<meta http-equiv='Content-Type' content='text/html; charset=utf-8'>"
    putStr $ "<!--" ++ show chains ++ "-->"
    putStr $ "<!--" ++ T.unpack gitLog ++ "-->"
    putStr $ renderHtml $ formatTables tables
