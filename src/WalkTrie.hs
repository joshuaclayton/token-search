module WalkTrie
    ( processText
    , aggregateResults
    ) where

import Control.Arrow ((&&&))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Trie

aggregateResults :: [Map.Map String Int] -> Map.Map String Int
aggregateResults = foldl1 (Map.unionWith (+))

advanceStates :: [WalkedNode] -> Char -> ([WalkedNode], [String])
advanceStates nodes char =
    mapMaybe clearNode &&& concatMap walkedTerminalResult $ walkedNodes
  where
    walkedNodes = map (walk char) nodes
    clearNode (Ended _) = Nothing
    clearNode a = Just a

processText :: Trie -> String -> Map.Map String Int
processText trie body = snd $ foldl f ([], Map.empty) body
  where
    f (state, map') char =
        let (newState, words') = advanceStates (state ++ [root trie]) char
            newMap m word = Map.insertWith (+) word 1 m
         in (newState, foldl newMap map' words')

data WalkedNode
    = Unwalked Trie
    | Walked String
             Node
    | Ended String
    deriving (Show)

root :: Trie -> WalkedNode
root = Unwalked

walk :: Char -> WalkedNode -> WalkedNode
walk char node =
    case node of
        Ended x -> Ended x
        Unwalked trie ->
            case findNodeFromTrie trie char of
                Nothing -> Ended [char]
                Just node' -> Walked [char] node'
        Walked string node' ->
            case findNodeFromChildren node' char of
                Nothing -> Ended $ string ++ [char]
                Just node'' -> Walked (string ++ [char]) node''

walkedTerminalResult :: WalkedNode -> [String]
walkedTerminalResult node =
    case node of
        Unwalked _ -> []
        Ended _ -> []
        Walked base node' -> [base | isTerminal node']
