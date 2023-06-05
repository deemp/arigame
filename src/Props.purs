module Props where


import Data.Monoid ((<>))
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HP

attr_ ∷ ∀ (a ∷ Row Type) (b ∷ Type). String → String → IProp a b
attr_ k v = HP.attr (AttrName k) v


ariaLabelledby :: forall a12 b13. String -> IProp a12 b13
ariaLabelledby = attr_ "aria-labelledby"

ariaLabel :: forall a15 b16. String -> IProp a15 b16
ariaLabel = attr_ "aria-label"

dataBsDismiss :: forall a3 b4. String -> IProp a3 b4
dataBsDismiss = attr_ "data-bs-dismiss"

ariaControls :: forall a18 b19. String -> IProp a18 b19
ariaControls = attr_ "aria-controls"

dataBsToggle :: forall a9 b10. String -> IProp a9 b10
dataBsToggle = attr_ "data-bs-toggle"

dataBsTarget :: forall a6 b7. String -> IProp a6 b7
dataBsTarget s = attr_ "data-bs-target" ("#" <> s)