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

import Juggler.Parse
import Juggler.Types

sh command = do
    putStrLn command
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

fileChains commits = [map (fillCommit fileSet . onlyFiles fileSet) commits| fileSet <- renameSets]
    where
        deltas = concatMap c_deltas commits
        allFiles = concatMap (\d -> [fd_source d, fd_dest d]) deltas
        renameSets = clusterSets $ map deltaFiles deltas

main = do
    args <- getArgs
    gitLog <- sh $ L.intercalate " " ("git log -p -U0 --pretty='format:%H%n----%n%B%n----%n'":args)
    putStrLn $ show $ head $ fileChains $ fromJust $ maybeResult $ newlineTerminate $ parse (orderedCommits <* endOfInput) gitLog
