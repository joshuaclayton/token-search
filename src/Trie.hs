module Trie
    ( buildTrieWithTokens
    , isTerminal
    , findNodeFromTrie
    , findNodeFromChildren
    , Trie
    , Node
    ) where

import qualified Data.List as L
import Util

newtype Trie =
    Root [Node]
    deriving (Show)

data Node
    = Terminal !Char
               ![Node]
    | NonTerminal !Char
                  ![Node]
    deriving (Show)

buildTrieWithTokens :: [String] -> Trie
buildTrieWithTokens = finalize . foldl (flip add) buildTrie

finalize :: Trie -> Trie
finalize (Root nodes) = Root $ recursiveMergeNodes nodes

findNodeFromTrie :: Trie -> Char -> Maybe Node
findNodeFromTrie (Root nodes) char = L.find (\n -> nodeChar n == char) nodes

findNodeFromChildren :: Node -> Char -> Maybe Node
findNodeFromChildren node char =
    L.find (\n -> nodeChar n == char) $ nodeChildren node

isTerminal :: Node -> Bool
isTerminal node =
    case node of
        Terminal _ _ -> True
        NonTerminal _ _ -> False

nodeChar :: Node -> Char
nodeChar node =
    case node of
        Terminal char _ -> char
        NonTerminal char _ -> char

nodeChildren :: Node -> [Node]
nodeChildren node =
    case node of
        Terminal _ xs -> xs
        NonTerminal _ xs -> xs

nodeConstructor :: [Node] -> (Char -> [Node] -> Node)
nodeConstructor nodes =
    case nodes of
        [] -> NonTerminal
        xs ->
            if any isTerminal xs
                then Terminal
                else NonTerminal

recursiveMergeNodes :: [Node] -> [Node]
recursiveMergeNodes nodes =
    map (\(c, xs) ->
             let constructor = nodeConstructor xs
              in constructor c $ recursiveMergeNodes $ concatMap nodeChildren xs)
        charAndNodes
  where
    charAndNodes = groupBy nodeChar nodes

addNode :: Trie -> Node -> Trie
addNode (Root nodes) node = Root $ nodes ++ [node]

buildTrie :: Trie
buildTrie = Root []

add :: String -> Trie -> Trie
add value trie =
    case value of
        "" -> trie
        (x:xs) -> addNode trie $ createNode x xs

createNode :: Char -> String -> Node
createNode char rest =
    case rest of
        [] -> Terminal char []
        (x:xs) -> NonTerminal char [createNode x xs]
