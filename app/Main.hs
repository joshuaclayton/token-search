module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.TokenSearch as TokenSearch
import System.Process (readProcess)

main :: MonadIO m => m ()
main = do
    tokens <- calculateTokens
    results <-
        TokenSearch.calculateResults tokens =<< TokenSearch.calculateFileNames
    liftIO $ BS.putStr $ A.encode results

calculateTokens :: MonadIO m => m [T.Text]
calculateTokens =
    tokensFromTags . T.pack <$> liftIO (readProcess "cat" [".git/tags"] [])

tokensFromTags :: T.Text -> [T.Text]
tokensFromTags = L.nub . tokenLocations
  where
    tokenLocations = map (head . T.splitOn "\t") . T.lines
