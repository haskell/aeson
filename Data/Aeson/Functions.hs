module Data.Aeson.Functions
    (
      transformMap
    ) where

import Control.Arrow ((***))
import qualified Data.Map as M

-- | Transform one map into another.  The ordering of keys must be
-- preserved.
transformMap :: (Ord k2) => (k1 -> k2) -> (v1 -> v2)
             -> M.Map k1 v1 -> M.Map k2 v2
transformMap fk fv = M.fromAscList . map (fk *** fv) . M.toAscList
