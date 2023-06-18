module Props where

import Data.Monoid ((<>))
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HP

attr_ ∷ ∀ (a ∷ Row Type) (b ∷ Type). String → String → IProp a b
attr_ k v = HP.attr (AttrName k) v

aAriaLabelledby :: forall a12 b13. String -> IProp a12 b13
aAriaLabelledby = attr_ "aria-labelledby"

aAriaLabel :: forall a15 b16. String -> IProp a15 b16
aAriaLabel = attr_ "aria-label"

aDataBsDismiss :: forall a3 b4. String -> IProp a3 b4
aDataBsDismiss = attr_ "data-bs-dismiss"

aAriaControls :: forall a18 b19. String -> IProp a18 b19
aAriaControls = attr_ "aria-controls"

aDataBsToggle :: forall a9 b10. String -> IProp a9 b10
aDataBsToggle = attr_ "data-bs-toggle"

aDataBsTarget :: forall a6 b7. String -> IProp a6 b7
aDataBsTarget s = attr_ "data-bs-target" ("#" <> s)

aDisabled :: forall a9 b10. IProp a9 b10
aDisabled = attr_ "disabled" "true"
