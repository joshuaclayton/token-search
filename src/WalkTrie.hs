module WalkTrie
    ( processText
    , aggregateResults
    ) where

import Control.Arrow ((&&&))
import qualified Data.Map as Map
import Trie

data WalkedNode
    = Unwalked Trie
    | Walked String
             Node
    | Ended
    deriving (Show)

aggregateResults :: [Map.Map String Int] -> Map.Map String Int
aggregateResults = foldl1 (Map.unionWith (+))

processText :: Trie -> String -> Map.Map String Int
processText trie = snd . foldl f ([], Map.empty)
  where
    newTrie char =
        case findNodeFromTrie trie char of
            Nothing -> []
            Just _ -> [Unwalked trie]
    f (state, map') char =
        let (newState, words') = advanceStates char stateAndNewTrie
            stateAndNewTrie = newTrie char ++ state
            newMap m word = Map.insertWith (+) word 1 m
         in (newState, foldl newMap map' words')

advanceStates :: Char -> [WalkedNode] -> ([WalkedNode], [String])
advanceStates char =
    (id &&& concatMap walkedTerminalResult) .
    filter activeNode . map (walk char)

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
