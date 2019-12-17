module WalkTrie
    ( processText
    , trieToWalkedNode
    , processTextWithState
    , aggregateResults
    ) where

import Control.Arrow ((&&&))
import qualified Data.Map as Map
import qualified Data.Text as T
import Trie

data WalkedNode
    = Unwalked Trie
    | Walked String
             Node
    | Ended
    deriving (Show)

aggregateResults :: [Map.Map String Int] -> Map.Map String Int
aggregateResults = foldl1 (Map.unionWith (+))

trieToWalkedNode :: Trie -> WalkedNode
trieToWalkedNode = Unwalked

processText :: Trie -> T.Text -> Map.Map String Int
processText trie = snd . processTextWithState [] trie

processTextWithState ::
       [WalkedNode] -> Trie -> T.Text -> ([WalkedNode], Map.Map String Int)
processTextWithState nodes trie = T.foldl f (nodes, Map.empty)
  where
    newTrie char =
        case findNodeFromTrie trie char of
            Nothing -> []
            Just _ -> [Unwalked trie]
    f (state, map') char = advanceStates char map' $ newTrie char ++ state

advanceStates ::
       Char
    -> Map.Map String Int
    -> [WalkedNode]
    -> ([WalkedNode], Map.Map String Int)
advanceStates char map' =
    (id &&& foldl newMap map' . concatMap walkedTerminalResult) .
    filter activeNode . map (walk char)
  where
    newMap m word = Map.insertWith (+) word 1 m

walk :: Char -> WalkedNode -> WalkedNode
walk _ Ended = Ended
walk char (Unwalked trie) =
    case findNodeFromTrie trie char of
        Nothing -> Ended
        Just node' -> Walked [char] node'
walk char (Walked string node) =
    case findNodeFromChildren node char of
        Nothing -> Ended
        Just node' -> Walked (string ++ [char]) node'

walkedTerminalResult :: WalkedNode -> [String]
walkedTerminalResult (Walked base node) = [base | isTerminal node]
walkedTerminalResult _ = []

activeNode :: WalkedNode -> Bool
activeNode Ended = False
activeNode _ = True
