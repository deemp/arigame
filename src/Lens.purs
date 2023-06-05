module Lens where

import Data.Lens (Lens)
import Data.Lens.Record (prop)
import Prim.Row (class Cons)
import Type.Prelude (class IsSymbol, Proxy(..))

p :: ∀ @l r1 r2 r a b. IsSymbol l ⇒ Cons l a r r1 ⇒ Cons l b r r2 ⇒ Lens (Record r1) (Record r2) a b
p = prop (Proxy @l)