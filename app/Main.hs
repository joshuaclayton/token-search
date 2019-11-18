{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad ((<$!>))
import qualified Data.ByteString.Lazy as C
import Data.Char (chr)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import System.Process (readProcess)
import Trie
import Util
import WalkTrie

main :: IO ()
main = do
    tokens <- calculateTokens
    filenames <- calculateFileNames
    print $ length tokens
    print $ length filenames
    let newTrie = buildTrieWithTokens tokens
    allResults <-
        mapM
            (\filename -> processWithContext newTrie <$!> readFileBS filename)
            filenames
    print $ Map.fromList $ map go $ groupBy firstFromTriple $ concat allResults
  where
    firstFromTriple (a, _, _) = a
    prepareForMap (_, b, c) = (b, c)
    go (token, xs) = (token, Map.fromList $ map prepareForMap xs)

calculateTokens :: IO [String]
calculateTokens = tokensFromTags <$> readProcess "cat" [".git/tags"] []

calculateFileNames :: IO [String]
calculateFileNames = lines <$> readProcess "git" ["ls-files"] []

readFileBS :: String -> IO (String, String)
readFileBS filename =
    (, filename) . map (chr . fromEnum) . C.unpack <$> C.readFile filename

processWithContext :: Trie -> (String, String) -> [(String, String, Int)]
processWithContext trie (input, filename) =
    map (\(token, ct) -> (token, filename, ct)) $
    Map.toList $ processText trie input

tokensFromTags :: String -> [String]
tokensFromTags = L.nub . tokenLocations
  where
    tokenLocations = map (token . T.splitOn "\t" . T.pack) . lines
    token = T.unpack . head
