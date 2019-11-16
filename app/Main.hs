{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad ((<$!>))
import qualified Data.ByteString.Lazy as C
import Data.Char (chr)
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import System.IO.Unsafe
import System.Process (readProcess)
import Trie
import WalkTrie

main :: IO ()
main = do
    tokens <- tokensFromTags <$> readProcess "cat" [".git/tags"] []
    let newTrie = buildTrieWithTokens tokens
    filenames <- lines <$> readProcess "git" ["ls-files"] []
    print $ length tokens
    print $ length filenames
    allResults <-
        mapM
            (\filename -> processWithContext newTrie <$!> readFileBS filename)
            filenames
    print $ Map.fromList $ map go $ groupBy (\(a, b, c) -> a) $
        concat allResults
  where
    prepareForMap (a, b, c) = (b, c)
    go (token, xs) = (token, Map.fromList $ map prepareForMap xs)

unsafeInterleaveMapIO f (x:xs) =
    unsafeInterleaveIO $ do
        y <- f x
        ys <- unsafeInterleaveMapIO f xs
        return (y : ys)
unsafeInterleaveMapIO _ [] = return []

readFileBS filename =
    (, filename) . map (chr . fromEnum) . C.unpack <$!> C.readFile filename

processWithContext :: Trie -> (String, String) -> [(String, String, Int)]
processWithContext trie (input, filename) =
    map (\(token, ct) -> (token, filename, ct)) $ Map.toList $
    processText trie input

tokensFromTags :: String -> [String]
tokensFromTags = L.nub . tokenLocations
  where
    tokenLocations = map (token . T.splitOn "\t" . T.pack) . lines
    token = T.unpack . head

groupBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupBy f =
    map (f . head &&& id) . L.groupBy ((==) `on` f) . L.sortBy (compare `on` f)
