module Juggler.PaddedList where

import Prelude hiding (take, drop, splitAt, length)
import Control.Lens
import qualified Data.List as L
import Data.Maybe

import Juggler.Types

numbered = snd . L.mapAccumL numbered' 0
    where numbered' count (Padding a) = (count, (count, Padding a))
          numbered' count (Content a) = (count + 1, (count, Content a))

take n xs = map snd $ takeWhile ((< n) . fst) $ numbered xs
drop n xs = map snd $ dropWhile ((< n) . fst) $ numbered xs

splitAt :: Int -> PaddedList a b -> (PaddedList a b, PaddedList a b)
splitAt n xs = (map snd left, map snd right)
    where (left, right) = break ((>= n) . fst) $ numbered xs

contentToRaw :: PaddedList a b -> Int -> Int
contentToRaw xs n = fromMaybe (L.length xs) (L.findIndex ((== n) . fst) $ numbered xs)

replaceRawRange :: Range -> PaddedList a b -> PaddedList a b -> PaddedList a b
replaceRawRange = replaceRangeWith L.take L.drop

replaceContentRange :: (a -> Padded b c) -> Range -> [a] -> PaddedList b c -> PaddedList b c
replaceContentRange cons range repl = replaceRangeWith take drop range (map cons repl)

replaceRangeWith :: (Int -> [a] -> [a]) -> (Int -> [a] -> [a]) -> Range -> [a] -> [a] -> [a]
replaceRangeWith take drop range repl xs = before ++ repl ++ after
    where
        before = take (range ^. start) xs
        after = drop (range ^. end) xs

mapRawRange = mapRangeWith L.take L.drop
mapContentRange range fn = mapRangeWith take drop range (fmap fn)

mapRangeWith :: (Int -> [a] -> [a]) -> (Int -> [a] -> [a]) -> Range -> (a -> a) -> [a] -> [a]
mapRangeWith take drop range fn xs = before ++ map fn toChange ++ after
    where
        before = take (range ^. start) xs
        toChange = take (range ^. length) $ drop (range ^. start) xs
        after = drop (range ^. end) xs

insertRaw pos = replaceRawRange (Range pos 0)

insertContent :: (a -> Padded b c) -> Int -> [a] -> PaddedList b c -> PaddedList b c
insertContent cons pos = replaceContentRange cons (Range pos 0)
