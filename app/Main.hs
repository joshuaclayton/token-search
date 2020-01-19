module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as Map
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.TokenSearch as TokenSearch
import Numeric.Natural (Natural)
import System.Process (readProcess)

newtype Results = Results
    { value :: Map.HashMap T.Text (Map.HashMap TokenSearch.FilePathWithMetadata Natural)
    }

main :: MonadIO m => m ()
main = do
    tokens <- calculateTokens
    results <-
        TokenSearch.calculateResults tokens =<< TokenSearch.calculateFileNames
    liftIO $ BS.putStr $ A.encode $ Results results

calculateTokens :: MonadIO m => m [T.Text]
calculateTokens =
    tokensFromTags . T.pack <$> liftIO (readProcess "cat" [".git/tags"] [])

tokensFromTags :: T.Text -> [T.Text]
tokensFromTags = L.nub . tokenLocations
  where
    tokenLocations = map (head . T.splitOn "\t") . T.lines

instance A.ToJSON Results where
    toJSON (Results map') = A.object $ toJSONTokenAndValue <$> Map.toList map'
      where
        toJSONTokenAndValue (t, m) =
            t .= (toJSONFilePathAndOccurrences <$> Map.toList m)
        toJSONFilePathAndOccurrences (fp, occ) =
            A.object
                [ "filePath" .= TokenSearch.filePath fp
                , "md5" .= show (TokenSearch.md5 fp)
                , "occurrences" .= occ
                ]
