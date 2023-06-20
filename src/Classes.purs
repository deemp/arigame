module Classes where

import Prelude

class HasSymbol a where
  getSymbol :: a -> String
