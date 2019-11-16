module WalkTrie
    ( processText
    , aggregateResults
    ) where

import qualified Data.List as L
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Trie
import Util

aggregateResults :: [Map.Map String Int] -> Map.Map String Int
aggregateResults = foldl1 (Map.unionWith (+))

advanceStates :: [WalkedNode] -> Char -> ([WalkedNode], [String])
advanceStates nodes char =
    (mapMaybe clearNode walkedNodes, concatMap walkedTerminalResult walkedNodes)
  where
    walkedNodes = map (walk char) nodes
    clearNode node =
        case node of
            Ended _ -> Nothing
            a -> Just a

processText :: Trie -> String -> Map.Map String Int
processText trie body = snd $ foldl f ([], Map.empty) body
  where
    f (state, map') char =
        let (newState, words) = advanceStates (state ++ [root trie]) char
            newMap m word = Map.insertWith (+) word 1 m
         in (newState, foldl newMap map' words)

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
        Unwalked (Root nodes) ->
            case L.find (\n -> nodeChar n == char) nodes of
                Nothing -> Ended [char]
                Just node' -> Walked [char] node'
        Walked string node' ->
            case L.find (\n -> nodeChar n == char) (nodeChildren node') of
                Nothing -> Ended $ string ++ [char]
                Just node'' -> Walked (string ++ [char]) node''

walkedTerminalResult :: WalkedNode -> [String]
walkedTerminalResult node =
    case node of
        Unwalked _ -> []
        Ended _ -> []
        Walked base node -> [base | isTerminal node]

thing :: String -> Node -> Maybe String
thing accumulated node =
    case node of
        Terminal c _ -> Just $ accumulated ++ [c]
        _ -> Nothing

thing2 :: String -> Node -> [Maybe String]
thing2 acc node = results
  where
    acc' = acc ++ [nodeChar node]
    children = nodeChildren node
    results =
        concatMap (\child -> thing acc' child : thing2 acc' child) children
