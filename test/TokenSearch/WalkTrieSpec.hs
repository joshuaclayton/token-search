module TokenSearch.WalkTrieSpec where

import qualified Data.Map as Map
import Test.Hspec
import Trie
import WalkTrie

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "WalkTrie.processTextWithState" $
    it "allows for processing text with remaining state" $ do
        let trie = buildTrieWithTokens ["foo", "foobar", "Bar"]
        let (walkedNodes, outcome1) = processTextWithState [] trie "foob"
        let (_, outcome2) = processTextWithState walkedNodes trie "arfooBarbar"
        let outcome = aggregateResults [outcome1, outcome2]
        Map.lookup "foo" outcome `shouldBe` Just 2
        Map.lookup "foobar" outcome `shouldBe` Just 1
        Map.lookup "Bar" outcome `shouldBe` Just 1
        Map.lookup "non-token" outcome `shouldBe` Nothing
