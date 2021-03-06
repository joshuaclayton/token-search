module Data.TokenSearch
    ( calculateResults
    , calculateFileNames
    ) where

import Conduit
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Bifunctor as BF
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import qualified Data.Streaming.FileRead as FR
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.TokenSearch.Trie
import Data.TokenSearch.WalkTrie
import Numeric.Natural (Natural)
import System.Process (readProcess)

calculateFileNames :: MonadIO m => m [String]
calculateFileNames = lines <$> liftIO (readProcess "git" ["ls-files"] [])

calculateResults ::
       MonadIO m
    => [T.Text]
    -> [FilePath]
    -> m (Map.HashMap T.Text (Map.HashMap FilePath Natural))
calculateResults tokens filenames = do
    let newTrie = buildTrieWithTokens tokens
    transformMap . Map.unions <$> processAllFiles filenames newTrie

processAllFiles ::
       MonadIO m
    => [FilePath]
    -> Trie
    -> m [Map.HashMap FilePath (Map.HashMap T.Text Natural)]
processAllFiles filenames trie =
    liftIO $
    runConduitRes $
    pathAndContentsSource filenames .| processTextC trie .| sinkList

pathAndContentsSource ::
       MonadResource m => [FilePath] -> ConduitT () (FilePath, T.Text) m ()
pathAndContentsSource filenames =
    yieldMany filenames .| awaitForever sourceFileWithFilename .|
    mapC (BF.second lenientUtf8Decode)

lenientUtf8Decode :: BS.ByteString -> T.Text
lenientUtf8Decode = T.decodeUtf8With T.lenientDecode

sourceFileWithFilename ::
       MonadResource m => FilePath -> ConduitT i (FilePath, BS.ByteString) m ()
sourceFileWithFilename fp =
    bracketP (FR.openFile fp) FR.closeFile (loop BS.empty)
  where
    loop acc h = do
        bs <- liftIO $ FR.readChunk h
        if BS.null bs
            then yield (fp, acc)
            else loop (BS.append acc bs) h

processTextC ::
       Monad m
    => Trie
    -> ConduitT (FilePath, T.Text) (Map.HashMap FilePath (Map.HashMap T.Text Natural)) m ()
processTextC trie = mapC (processTextWithFilename trie)

processTextWithFilename ::
       Trie
    -> (FilePath, T.Text)
    -> Map.HashMap FilePath (Map.HashMap T.Text Natural)
processTextWithFilename trie (filename, input) =
    Map.singleton filename $ processText trie input

transformMap ::
       (Hashable a, Hashable b, Eq a, Eq b)
    => Map.HashMap a (Map.HashMap b c)
    -> Map.HashMap b (Map.HashMap a c)
transformMap = Map.foldlWithKey' f Map.empty
  where
    f tokenToFilenamesAcc filename =
        Map.foldlWithKey'
            (\acc token count ->
                 Map.insertWith
                     Map.union
                     token
                     (Map.singleton filename count)
                     acc)
            tokenToFilenamesAcc
