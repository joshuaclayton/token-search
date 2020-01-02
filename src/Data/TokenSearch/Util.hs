module Data.TokenSearch.Util
    ( groupBy
    ) where

import Control.Arrow ((&&&))
import Data.Function (on)
import qualified Data.List as L

groupBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupBy f =
    map (f . head &&& id) . L.groupBy ((==) `on` f) . L.sortBy (compare `on` f)
