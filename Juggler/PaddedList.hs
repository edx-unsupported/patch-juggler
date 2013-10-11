module Juggler.PaddedList where

import Prelude hiding (take, drop, splitAt)
import qualified Data.List as L

import Juggler.Types

numbered = snd . L.mapAccumL numbered' 0
    where numbered' count Padding = (count, (count, Padding))
          numbered' count (Content a) = (count + 1, (count, Content a))

take n xs = map snd $ takeWhile ((< n) . fst) $ numbered xs
drop n xs = map snd $ dropWhile ((< n) . fst) $ numbered xs

splitAt :: Int -> PaddedList a -> (PaddedList a, PaddedList a)
splitAt n xs = (map snd left, map snd right)
    where (left, right) = break ((>= n) . fst) $ numbered xs
--take 0 _ = []
--take _ [] = []
--take n (Padding:xs) = Padding:take n xs
--take n (Content a:xs) = Content a:take (n-1) xs

--drop 0 xs = xs
--drop _ [] = []
--drop n (Padding:xs) = drop n xs
--drop n (Content _:xs) = drop (n-1) xs

replaceRawRange = replaceRangeWith L.take L.drop
replaceContentRange range repl = replaceRangeWith take drop range (map Content repl)

replaceRangeWith take drop range repl xs = before ++ repl ++ after
    where
        before = take (r_start range) xs
        after = drop (r_end range) xs

mapRawRange = mapRangeWith L.take L.drop
mapContentRange range fn = mapRangeWith take drop range (fmap fn)

mapRangeWith take drop range fn xs = before ++ map fn toChange ++ after
    where
        before = take (r_start range) xs
        toChange = take (r_length range) $ drop (r_start range) xs
        after = drop (r_end range) xs

insertRawPadding = insertPaddingWith L.splitAt
insertContentPadding = insertPaddingWith splitAt

insertPaddingWith splitAt pos amount xs = before ++ (replicate amount Padding) ++ after
    where
        (before, after) = splitAt pos xs

insertRaw pos = replaceRawRange (Range pos 0)
insertContent pos = replaceContentRange (Range pos 0)
