{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (take)
import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Text (Text)
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.Char
import System.Environment
import System.Process
import qualified Data.List as L

data Commit = Commit {
    p_sha :: Text,
    p_msg :: Text,
    p_deltas :: [FileDelta]
    }

data FileDelta = FileDelta {
    fd_source :: Text,
    fd_dest :: Text,
    fd_hunks :: [Hunk]
    }

data Hunk = Hunk {
    h_sourceStart :: Int,
    h_sourceEnd :: Int,
    h_destStart :: Int,
    h_destEnd :: Int,
    h_output :: [Text]
    }

line = do
    ln <- takeTill isEndOfLine
    endOfLine
    return ln

parseCommit = do
    sha <- "commit " .*> take 40
    msg <- many line
    endOfLine
    deltas <- many parseDiffs
    return $ Commit sha (T.unlines msg) deltas

parseDiffs = do
    string "diff --git "
    source <- "a/" .*> takeTill isSpace
    dest <- "b/" .*> line
    line -- index $hash..$hash perms
    line -- --- source
    line -- +++ dest
    hunks <- many parseHunk
    return $ FileDelta source dest hunks

parseHunk = do
    string "@@ "
    sourceStart <- "-" .*> decimal
    sourceEnd <- "," .*> decimal
    string " "
    destStart <- "+" .*> decimal
    destEnd <- "," .*> decimal
    string " @@"
    line
    skipMany ("-" >> line)
    output <- many ("+" .*> line)
    return $ Hunk sourceStart sourceEnd destStart destEnd output

main = do
    args <- getArgs
    runCommand $ L.intercalate " " ("git log -p -U0 --pretty='format:%H%n%B'":args)

{-
911b1582e31256d98dd1c1f1f09085423d600554
    WIP

diff --git a/rakelib/deprecated.rake b/rakelib/deprecated.rake
index fc7d67e..3de3fc4 100644
--- a/rakelib/deprecated.rake
+++ b/rakelib/deprecated.rake
@@ -17,0 +18,11 @@ end
+def deprecate_to_invoke(task, args, command)
+    arg_vals = []
+    task.arg_names.each do |name|
+        arg_vals << args[name]
+    end
+    puts("#{task.name}[#{arg_vals.join(', ')}] has been replaced by 'invoke #{command}'".red)
+    sleep(5)
+    sh("invoke #{command}")
+end
+
+
diff --git a/rakelib/docs.rake b/rakelib/docs.rake
index 025bee3..7e9a861 100644
--- a/rakelib/docs.rake
+++ b/rakelib/docs.rake
@@ -1,2 +0,0 @@
-require 'launchy'
-
@@ -4 +1,0 @@ require 'launchy'
-desc "Invoke sphinx 'make build' to generate docs."
@@ -7,17 +4,3 @@ task :builddocs, [:type, :quiet] do |t, args|
-    if args.type == 'dev'
-        path = "docs/developer"
-    elsif args.type == 'author'
-        path = "docs/course_authors"
-    elsif args.type == 'data'
-        path = "docs/data"
-    else
-        path = "docs"
-    end
-
-    Dir.chdir(path) do
-        if args.quiet == 'verbose'
-            sh('make html quiet=false')
-        else
-            sh('make html quiet=true')
-        end
-    end
+    verbosity = args.quiet == 'verbose' ? ' --verbose' : ''
+    type = args.type ? " --type #{args.type}" : ''
+    deprecate_to_invoke(t, args, "docs.builddocs#{type}#{verbosity}")
-}