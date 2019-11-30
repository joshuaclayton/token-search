module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as L
import qualified Data.Text as T
import System.Process (readProcess)
import qualified TokenSearch

main :: MonadIO m => m ()
main = do
    tokens <- calculateTokens
    filenames <- TokenSearch.calculateFileNames
    liftIO $ print $ length tokens
    liftIO $ print $ length filenames
    results <- TokenSearch.calculateResults tokens filenames
    liftIO $ print results

calculateTokens :: MonadIO m => m [String]
calculateTokens = tokensFromTags <$> liftIO (readProcess "cat" [".git/tags"] [])

tokensFromTags :: String -> [String]
tokensFromTags = L.nub . tokenLocations
  where
    tokenLocations = map (token . T.splitOn "\t" . T.pack) . lines
    token = T.unpack . head
