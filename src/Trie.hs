module Trie
    ( buildTrieWithTokens
    , isTerminal
    , findNodeFromTrie
    , findNodeFromChildren
    , Trie
    , Node
    ) where

import qualified Data.Map as Map

type NodeMap = Map.Map Char Node

newtype Trie =
    Root NodeMap
    deriving (Show)

data Node
    = Terminal !NodeMap
    | NonTerminal !NodeMap
    deriving (Show)

buildTrieWithTokens :: [String] -> Trie
buildTrieWithTokens = foldl (flip add) buildTrie

findNodeFromTrie :: Trie -> Char -> Maybe Node
findNodeFromTrie (Root nodes) char = Map.lookup char nodes

findNodeFromChildren :: Node -> Char -> Maybe Node
findNodeFromChildren node char = Map.lookup char $ nodeChildren node

mergeSameNodes :: Node -> Node -> Node
mergeSameNodes (NonTerminal xs) (NonTerminal ys) =
    NonTerminal $ mergeNodeMaps xs ys
mergeSameNodes x y = Terminal $ mergeNodeMaps (nodeChildren x) (nodeChildren y)

mergeNodeMaps :: NodeMap -> NodeMap -> NodeMap
mergeNodeMaps = Map.unionWith mergeSameNodes

isTerminal :: Node -> Bool
isTerminal (Terminal _) = True
isTerminal (NonTerminal _) = False

nodeChildren :: Node -> NodeMap
nodeChildren (Terminal xs) = xs
nodeChildren (NonTerminal xs) = xs

addNode :: Trie -> Char -> Node -> Trie
addNode (Root nodes) char node =
    Root $ Map.insertWith mergeSameNodes char node nodes

buildTrie :: Trie
buildTrie = Root Map.empty

add :: String -> Trie -> Trie
add "" trie = trie
add (x:xs) trie = addNode trie x $ createNode xs

createNode :: String -> Node
createNode rest =
    case rest of
        [] -> Terminal Map.empty
        (x:xs) -> NonTerminal $ Map.singleton x $ createNode xs
