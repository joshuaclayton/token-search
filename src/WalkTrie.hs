module WalkTrie
    ( processText
    , aggregateResults
    ) where

import Control.Arrow ((&&&))
import qualified Data.Map as Map
import qualified Data.Maybe as M
import qualified Data.Text as T
import Trie

data WalkedNode
    = Unwalked Trie
    | Walked String
             Node
    deriving (Show)

aggregateResults :: [Map.Map String Int] -> Map.Map String Int
aggregateResults = foldl1 (Map.unionWith (+))

processText :: Trie -> T.Text -> Map.Map String Int
processText trie = snd . T.foldl f ([], Map.empty)
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
    M.mapMaybe (walk char)
  where
    newMap m word = Map.insertWith (+) word 1 m

{-# INLINE walk #-}
walk :: Char -> WalkedNode -> Maybe WalkedNode
walk char (Unwalked trie) =
    case findNodeFromTrie trie char of
        Nothing -> Nothing
        Just node' -> Just $ Walked [char] node'
walk char (Walked string node) =
    case findNodeFromChildren node char of
        Nothing -> Nothing
        Just node' -> Just $ Walked (string ++ [char]) node'

walkedTerminalResult :: WalkedNode -> [String]
walkedTerminalResult (Walked base node) = [base | isTerminal node]
walkedTerminalResult _ = []
