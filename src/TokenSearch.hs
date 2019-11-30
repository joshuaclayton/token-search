{-# LANGUAGE TupleSections #-}

module TokenSearch
    ( calculateResults
    , calculateFileNames
    ) where

import Control.Monad ((<$!>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as C
import Data.Char (chr)
import qualified Data.Map.Strict as Map
import System.Process (readProcess)
import Trie
import WalkTrie

calculateFileNames :: MonadIO m => m [String]
calculateFileNames = lines <$> liftIO (readProcess "git" ["ls-files"] [])

calculateResults ::
       MonadIO m
    => [String]
    -> [FilePath]
    -> m (Map.Map String (Map.Map String Int))
calculateResults tokens filenames = do
    let newTrie = buildTrieWithTokens tokens
    transformMap . Map.unions <$>
        mapM
            (\filename ->
                 processTextWithFilename newTrie <$!> readFileBS filename)
            filenames

readFileBS :: MonadIO m => String -> m (String, String)
readFileBS filename =
    (, filename) . map (chr . fromEnum) . C.unpack <$>
    liftIO (C.readFile filename)

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
