module TokenSearch
    ( calculateResults
    , calculateFileNames
    ) where

import Conduit
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Bifunctor as BF
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Streaming.FileRead as FR
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
    pathAndContentsSource filenames .| processTextC trie .|
    mapC (Map.map aggregateResults) .|
    sinkList

pathAndContentsSource ::
       MonadResource m => [FilePath] -> ConduitT () (FilePath, T.Text) m ()
pathAndContentsSource filenames =
    yieldMany filenames .| awaitForever sourceFileWithFilename .|
    mapC (BF.second lenientUtf8Decode)

lenientUtf8Decode :: BS.ByteString -> T.Text
lenientUtf8Decode = T.decodeUtf8With T.lenientDecode

sourceFileWithFilename ::
       MonadResource m => FilePath -> ConduitT i (FilePath, BS.ByteString) m ()
sourceFileWithFilename fp = bracketP (FR.openFile fp) FR.closeFile loop
  where
    loop h = do
        bs <- liftIO $ FR.readChunk h
        unless (BS.null bs) $ do
            yield (fp, bs)
            loop h

processTextC ::
       Monad m
    => Trie
    -> ConduitT (FilePath, T.Text) (Map.Map FilePath [Map.Map String Int]) m ()
processTextC trie = loop Nothing [] [Map.empty]
  where
    loop currentFilename walkedNodes mapResults = do
        v <- await
        case v of
            Just (fp, input) ->
                case currentFilename of
                    Nothing -> do
                        let (newWalkedNodes, newMapResults) =
                                processTextWithState walkedNodes trie input
                        loop (Just fp) newWalkedNodes [newMapResults]
                    Just currentFilename' ->
                        if fp /= currentFilename'
                            then do
                                yield $
                                    Map.singleton currentFilename' mapResults
                                let (newWalkedNodes, newMapResults) =
                                        processTextWithState
                                            walkedNodes
                                            trie
                                            input
                                loop (Just fp) newWalkedNodes [newMapResults]
                            else do
                                let (newWalkedNodes, newMapResults) =
                                        processTextWithState
                                            walkedNodes
                                            trie
                                            input
                                loop
                                    (Just fp)
                                    newWalkedNodes
                                    (newMapResults : mapResults)
            Nothing ->
                case currentFilename of
                    Nothing -> return ()
                    Just currentFilename' -> do
                        yield $ Map.singleton currentFilename' mapResults
                        return ()

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
