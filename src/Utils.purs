module Utils where

import Prelude

import Data.Array (intercalate)
import Data.Lens.Barlow (barlow)
import Data.Maybe (maybe)
import Effect.Exception (throw)
import Halogen (ClassName(..))
import Halogen.HTML (IProp)
import Halogen.HTML.Properties (class_)
import Unsafe.Coerce (unsafeCoerce)

singletonIf :: forall a. Boolean -> a -> Array a
singletonIf cond x = if cond then [ x ] else []

arrayIf :: forall a. Boolean -> Array a -> Array a
arrayIf cond xs = if cond then xs else []

classes_ :: forall (t1 :: Row Type) (t2 :: Type). Array String -> Array (IProp (class :: String | t1) t2)
classes_ a = [ class_ $ ClassName $ intercalate " " a ]

error :: ∀ a. String -> a
error = unsafeCoerce <<< throw

bw = barlow

fromJust msg = maybe (error msg) identity