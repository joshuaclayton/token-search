{-# LANGUAGE TupleSections #-}

module TokenSearch
    ( calculateResults
    , calculateFileNames
    ) where

import Control.Monad ((<$!>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
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

readFileBS :: MonadIO m => FilePath -> m (FilePath, T.Text)
readFileBS filename =
    (filename, ) . lenientUtf8Decode <$> liftIO (BS.readFile filename)

processTextWithFilename ::
       Trie -> (FilePath, T.Text) -> Map.Map FilePath (Map.Map String Int)
processTextWithFilename trie (filename, input) =
    Map.singleton filename $ processText trie input

lenientUtf8Decode :: BS.ByteString -> T.Text
lenientUtf8Decode = T.decodeUtf8With T.lenientDecode

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
