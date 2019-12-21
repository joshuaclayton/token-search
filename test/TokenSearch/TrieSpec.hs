module TokenSearch.TrieSpec where

import qualified Data.HashMap.Strict as Map
import Test.Hspec
import Trie
import WalkTrie

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "WalkTrie" $ do
        it "allows for processing text" $ do
            let trie = buildTrieWithTokens ["foo", "foobar", "Bar"]
            let outcome = processText trie "foobarfooBarbar"
            Map.lookup "foo" outcome `shouldBe` Just 2
            Map.lookup "foobar" outcome `shouldBe` Just 1
            Map.lookup "Bar" outcome `shouldBe` Just 1
            Map.lookup "non-token" outcome `shouldBe` Nothing
        it "supports insertion of shorter words later" $ do
            let trie = buildTrieWithTokens ["foobar", "foo"]
            let outcome = processText trie "foobar"
            Map.lookup "foo" outcome `shouldBe` Just 1
            Map.lookup "foobar" outcome `shouldBe` Just 1
        it "allows for aggregating results" $ do
            let trie = buildTrieWithTokens ["foo", "foobar", "Bar"]
            let outcome = processText trie "foobarfooBarbar"
            let otherOutcome = processText trie "BarBarBar foob"
            let combinedOutcome = aggregateResults [outcome, otherOutcome]
            Map.lookup "foo" combinedOutcome `shouldBe` Just 3
            Map.lookup "foobar" combinedOutcome `shouldBe` Just 1
            Map.lookup "Bar" combinedOutcome `shouldBe` Just 4
            Map.lookup "non-token" combinedOutcome `shouldBe` Nothing
