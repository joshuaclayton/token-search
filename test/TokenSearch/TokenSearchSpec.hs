module TokenSearch.TokenSearchSpec where

import qualified Data.Map as Map
import Test.Hspec
import TokenSearch

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
        Map.toList <$>
            Map.lookup "Person" results `shouldBe`
            Just
                [ ("test/data/long_file.rb", 3)
                , ("test/data/person.rb", 1)
                , ("test/data/person_spec.rb", 3)
                ]
        Map.toList <$>
            Map.lookup "Place" results `shouldBe`
            Just [("test/data/place.rb", 1)]
