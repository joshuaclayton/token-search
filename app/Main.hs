{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad ((<$!>))
import qualified Data.ByteString.Lazy as C
import Data.Char (chr)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Process (readProcess)
import Trie
import WalkTrie

main :: IO ()
main = do
    tokens <- calculateTokens
    filenames <- calculateFileNames
    print $ length tokens
    print $ length filenames
    let newTrie = buildTrieWithTokens tokens
    allResults <-
        Map.unions <$>
        mapM
            (\filename ->
                 processTextWithFilename newTrie <$!> readFileBS filename)
            filenames
    print $ transformMap allResults

calculateTokens :: IO [String]
calculateTokens = tokensFromTags <$> readProcess "cat" [".git/tags"] []

calculateFileNames :: IO [String]
calculateFileNames = lines <$> readProcess "git" ["ls-files"] []

readFileBS :: String -> IO (String, String)
readFileBS filename =
    (, filename) . map (chr . fromEnum) . C.unpack <$> C.readFile filename

processTextWithFilename ::
       Trie -> (String, String) -> Map.Map String (Map.Map String Int)
processTextWithFilename trie (input, filename) =
    Map.singleton filename $ processText trie input

transformMap ::
       (Ord a, Ord b) => Map.Map a (Map.Map b Int) -> Map.Map b (Map.Map a Int)
transformMap = Map.foldlWithKey f Map.empty
  where
    f tokenToFilenamesAcc filename =
        Map.foldlWithKey
            (\acc token count ->
                 Map.insertWith
                     Map.union
                     token
                     (Map.singleton filename count)
                     acc)
            tokenToFilenamesAcc

tokensFromTags :: String -> [String]
tokensFromTags = L.nub . tokenLocations
  where
    tokenLocations = map (token . T.splitOn "\t" . T.pack) . lines
    token = T.unpack . head
