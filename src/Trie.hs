module Trie
    ( buildTrieWithTokens
    , nodeChar
    , nodeChildren
    , isTerminal
    , Trie(..)
    , Node(..)
    ) where

import Control.Arrow ((&&&))
import Data.Function ((&))
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T

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

matchToken :: Trie -> String -> [String]
matchToken trie = walkedResults . foldl (flip walk) (root trie)

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
        Walked base node ->
            if isTerminal node
                then [base]
                else []

walkedResults :: WalkedNode -> [String]
walkedResults node =
    case node of
        Unwalked _ -> []
        Ended _ -> []
        Walked base node ->
            let current =
                    if isTerminal node
                        then [base]
                        else []
                children = catMaybes $ thing2 (L.init base) node
             in current ++ children

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
        concatMap (\child -> [thing acc' child] ++ thing2 acc' child) $ children

groupBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupBy f =
    map (f . head &&& id) . L.groupBy ((==) `on` f) . L.sortBy (compare `on` f)
