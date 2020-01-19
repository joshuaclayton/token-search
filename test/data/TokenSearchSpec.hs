module Data.TokenSearchSpec where

import qualified Data.Bifunctor as BF
import qualified Data.HashMap.Strict as Map
import qualified Data.List as L
import Data.TokenSearch
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "TokenSearch.calculateResults" $
    it "calculates the correct counts" $ do
        let tokens = ["Person", "name", "age", "Place", "latitude", "longitude"]
        let paths =
                [ "test/data/long_file.rb"
                , "test/data/place.rb"
                , "test/data/person.rb"
                , "test/data/person_spec.rb"
                ]
        results <- calculateResults tokens paths
        L.sort . fmap (BF.first filePath) . Map.toList <$>
            Map.lookup "Person" results `shouldBe`
            Just
                [ ("test/data/long_file.rb", 3)
                , ("test/data/person.rb", 1)
                , ("test/data/person_spec.rb", 3)
                ]
        fmap (BF.first filePath) . Map.toList <$>
            Map.lookup "Place" results `shouldBe`
            Just [("test/data/place.rb", 1)]
