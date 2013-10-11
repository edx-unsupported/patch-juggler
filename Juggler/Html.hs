{-# LANGUAGE OverloadedStrings #-}

module Juggler.Html where

import Prelude hiding (head, div)
import Text.Blaze.Internal hiding (Content)
import qualified Text.Blaze.Internal as I
import Text.Blaze.Html
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (style)
import Numeric
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Monoid
import Data.Ratio
import qualified Data.Text as T
import qualified Data.List as L
import Data.Function

import Juggler.Types

nbsp :: Html
nbsp = toHtml ("\160" :: String)

formatTextLine line = pre $ do
    toHtml (T.stripEnd line)
    nbsp

emptyLine = pre $ nbsp

formatLine :: Padded PaddingLine Line -> Html
formatLine (Padding Filler) = emptyLine
formatLine (Padding (Deleted gen)) = emptyLine ! class_ (mappend "gen-" (toValue gen))
formatLine (Content Elision) = div ! class_ "elision" $ toHtml ("\8942" :: String)
formatLine (Content (Original t)) = formatTextLine t
formatLine (Content (Dirty t)) = formatTextLine t
formatLine (Content (Added gen t)) = formatTextLine t ! class_ (mappend "gen-" (toValue gen))

elideLines lines = lines'
    where
        len = length lines
        lines' = if len > 3
            then (L.take 2 lines) ++ [Content Elision] ++ L.drop (len - 2) lines
            else lines

formatLineGroup :: PaddedList PaddingLine Line -> Html
formatLineGroup lines@(Padding Filler:rest) = div ! class_ "insert-line" $ mapM_ formatLine lines
formatLineGroup lines@(Padding (Deleted _):rest) = div ! class_ "del-line" $ mapM_ formatLine lines
formatLineGroup lines@(Content (Dirty _):rest) = div ! class_ "source-line dirty" $ mapM_ formatLine lines
formatLineGroup lines@(Content (Original _):rest) = div ! class_ "source-line original" $ mapM_ formatLine $ elideLines lines
formatLineGroup lines@(Content (Added _ _):rest) = div ! class_ "add-line" $ mapM_ formatLine lines

formatTableEntry :: PaddedList PaddingLine Line -> Html
formatTableEntry = td . mapM_ formatLineGroup . L.groupBy ((==) `on` lineType)

formatTable :: FileGrid -> Html
formatTable table = tr $ mapM_ formatTableEntry (reverse table)

formatTables :: [FileGrid] -> Html
formatTables = table . mapM_ formatTable

lineType (Padding Filler) = 0
lineType (Content (Dirty _)) = 1
lineType (Content (Original _)) = 2
lineType (Content (Added _ _)) = 3
lineType (Padding (Deleted _)) = 4

addColor max gen = hsv 112 0.2 (0.4 + (gen % max) * 0.5)
delColor max gen = hsv 0 0.5 (0.25 + (gen % max) * 0.7)

toCss r g b = (hex r) . (hex g) . (hex b)
    where hex = showHex . round . (256 *)

comment txt = I.Content $ PreEscaped $ String $ "<!--" ++ txt ++ "-->"

page comments maxGen tables = html $ do
    head $ do
        style $ do
            preEscapedToHtml ("td {vertical-align: top;}" :: String)
            preEscapedToHtml ("* {margin: 0;}" :: String)
            preEscapedToHtml (".insert-line {background-color: lightgray;}" :: String)
            preEscapedToHtml (".elision {text-align: center;}" :: String)
            preEscapedToHtml (".source-line.original {color: darkgray;}" :: String)
            sequence_ $ [
                preEscapedToHtml $ ".add-line .gen-" ++ show gen ++ "{background-color: #" ++ (uncurryRGB toCss $ addColor maxGen gen) ";}"
                | gen <- [0..maxGen]
                ]
            sequence_ $ [
                preEscapedToHtml $ ".del-line .gen-" ++ show gen ++ "{background-color: #" ++ (uncurryRGB toCss $ delColor maxGen gen) ";}"
                | gen <- [0..maxGen]
                ]
        meta ! httpEquiv "Content-Type" ! content "text/html; charset=utf-8"
        mapM_ comment comments
    body $ do
        formatTables tables