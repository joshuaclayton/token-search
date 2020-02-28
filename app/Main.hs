module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.TokenSearch as TokenSearch
import qualified Options.Applicative as OA
import System.Process (readProcess)

data CtagsSource
    = Stdin
    | FileSystem

main :: MonadIO m => m ()
main = runProgram =<< parseCLI

runProgram :: MonadIO m => CtagsSource -> m ()
runProgram ctagsSource = do
    tokens <- loadTags ctagsSource
    results <-
        TokenSearch.calculateResults tokens =<< TokenSearch.calculateFileNames
    liftIO $ BS.putStr $ A.encode results
  where
    loadTags Stdin = T.lines <$> stdinContent
    loadTags FileSystem = calculateTokens

calculateTokens :: MonadIO m => m [T.Text]
calculateTokens =
    tokensFromTags . T.pack <$> liftIO (readProcess "cat" [".git/tags"] [])

tokensFromTags :: T.Text -> [T.Text]
tokensFromTags = L.nub . tokenLocations
  where
    tokenLocations = map (head . T.splitOn "\t") . T.lines

parseCLI :: MonadIO m => m CtagsSource
parseCLI =
    liftIO $ OA.execParser (withInfo parseOptions pHeader pDescription pFooter)
  where
    pHeader = "token-search"
    pDescription =
        "token-search finds all occurrences of a set of tokens\
                   \ across a codebase"
    pFooter = "CLI USAGE: $ token-search"

withInfo :: OA.Parser a -> String -> String -> String -> OA.ParserInfo a
withInfo opts h d f =
    OA.info (OA.helper <*> opts) $ OA.header h <> OA.progDesc d <> OA.footer f

parseOptions :: OA.Parser CtagsSource
parseOptions = process <$> parseFromStdIn
  where
    process True = Stdin
    process False = FileSystem

parseFromStdIn :: OA.Parser Bool
parseFromStdIn = OA.switch $ OA.long "stdin" <> OA.help "Read tags from STDIN"

stdinContent :: MonadIO m => m T.Text
stdinContent = lenientUtf8Decode <$> getContentsLazy

getContentsLazy :: MonadIO m => m BS.ByteString
getContentsLazy = liftIO BS.getContents

lenientUtf8Decode :: BS.ByteString -> T.Text
lenientUtf8Decode = TL.toStrict . TL.decodeUtf8With T.lenientDecode
