module TokenSearch
    ( calculateResults
    , calculateFileNames
    ) where

import Conduit
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Process (readProcess)
import Trie
import WalkTrie

calculateFileNames :: MonadIO m => m [String]
calculateFileNames = lines <$> liftIO (readProcess "git" ["ls-files"] [])

calculateResults ::
       MonadIO m
    => [String]
    -> [FilePath]
    -> m (Map.Map String (Map.Map FilePath Int))
calculateResults tokens filenames = do
    let newTrie = buildTrieWithTokens tokens
    transformMap . Map.unions <$> processAllFiles filenames newTrie

processAllFiles ::
       MonadIO m
    => [FilePath]
    -> Trie
    -> m [Map.Map FilePath (Map.Map String Int)]
processAllFiles filenames trie =
    liftIO $
    runConduitRes $
    pathAndContentsSource filenames .| processTextC trie .| sinkList

pathAndContentsSource ::
       MonadResource m => [FilePath] -> ConduitT () (FilePath, T.Text) m ()
pathAndContentsSource filenames =
    getZipSource $
    (,) <$> ZipSource (yieldMany filenames) <*>
    ZipSource
        (yieldMany filenames .| awaitForever sourceFileBS .| decodeUtf8LenientC)

processTextC ::
       Monad m
    => Trie
    -> ConduitT (FilePath, T.Text) (Map.Map String (Map.Map String Int)) m ()
processTextC trie = Conduit.mapC (processTextWithFilename trie)

processTextWithFilename ::
       Trie -> (FilePath, T.Text) -> Map.Map FilePath (Map.Map String Int)
processTextWithFilename trie (filename, input) =
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
