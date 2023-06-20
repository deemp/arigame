module Utils where

import Prelude

import Data.Array (intercalate)
import Data.Lens (Lens)
import Data.Lens.Record (prop)
import Effect.Exception (throw)
import Halogen (ClassName(..))
import Halogen.HTML (IProp)
import Halogen.HTML.Properties (class_)
import Prim.Row (class Cons)
import Type.Prelude (class IsSymbol, Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

p :: ∀ @l r1 r2 r a b. IsSymbol l ⇒ Cons l a r r1 ⇒ Cons l b r r2 ⇒ Lens (Record r1) (Record r2) a b
p = prop (Proxy @l)

singletonIf :: forall a. Boolean -> a -> Array a
singletonIf cond x = if cond then [ x ] else []

arrayIf :: forall a. Boolean -> Array a -> Array a
arrayIf cond xs = if cond then xs else []

classes_ :: forall (t1 :: Row Type) (t2 :: Type). Array String -> Array (IProp (class :: String | t1) t2)
classes_ a = [ class_ $ ClassName $ intercalate " " a ]

error :: ∀ a. String -> a
error = unsafeCoerce <<< throw