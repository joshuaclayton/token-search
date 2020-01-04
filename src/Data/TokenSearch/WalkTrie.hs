module Data.TokenSearch.WalkTrie
    ( processText
    , aggregateResults
    ) where

import Control.Arrow ((&&&))
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import qualified Data.Maybe as M
import qualified Data.Text as T
import Data.TokenSearch.Trie
import Numeric.Natural (Natural)

data WalkedNode
    = Unwalked !Trie
    | Walked T.Text
             !Node
    deriving (Show)

aggregateResults ::
       (Eq a, Hashable a, Num b) => [Map.HashMap a b] -> Map.HashMap a b
aggregateResults = foldl1 (Map.unionWith (+))

processText :: Trie -> T.Text -> Map.HashMap T.Text Natural
processText trie = snd . T.foldl f ([], Map.empty)
  where
    newTrie char =
        case findNodeFromTrie trie char of
            Nothing -> []
            Just _ -> [Unwalked trie]
    f (state, map') char = advanceStates char map' $ newTrie char ++ state

advanceStates ::
       Char
    -> Map.HashMap T.Text Natural
    -> [WalkedNode]
    -> ([WalkedNode], Map.HashMap T.Text Natural)
advanceStates char map' =
    (id &&& foldl newMap map' . concatMap walkedTerminalResult) .
    M.mapMaybe (walk char)
  where
    newMap m word = Map.insertWith (+) word 1 m

walk :: Char -> WalkedNode -> Maybe WalkedNode
walk char (Unwalked trie) =
    case findNodeFromTrie trie char of
        Nothing -> Nothing
        Just node' -> Just $ Walked (T.singleton char) node'
walk char (Walked text node) =
    case findNodeFromChildren node char of
        Nothing -> Nothing
        Just node' -> Just $ Walked (T.snoc text char) node'

walkedTerminalResult :: WalkedNode -> [T.Text]
walkedTerminalResult (Walked base node) = [base | isTerminal node]
walkedTerminalResult _ = []
