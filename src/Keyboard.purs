module Keyboard where

import Prelude

import Actions (Button(..))
import CSSFrameworks.Bootstrap (bi, btn, btnSecondary, col, dFlex, justifyContentCenter, m1, p0, p1, pb2, row, rowCols3)
import CSSFrameworks.BootstrapIcons (biBackspaceFill)
import ClassNames (cButtonSquare, cKeyboard, cKeyboardCol)
import Common (getSymbol, ButtonId(..), Operator(..))
import Data.Array (concat, (..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)

data Action = ActionClicked Button

data Output = OutputClicked Button

type Slot id = forall query. H.Slot query Output id

component :: forall query input m. MonadAff m => H.Component query input Output m
component = H.mkComponent
  { initialState: const unit
  , render: const render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction :: Action -> H.HalogenM Unit Action () Output m Unit
  handleAction = case _ of
    ActionClicked button -> H.raise (OutputClicked button)

render :: forall m. H.ComponentHTML Action () m
render =
  let
    buttonClasses = [ btn, btnSecondary, cButtonSquare, p1, m1 ]
    mkElement e = HH.div [ classes [ col, p0 ] ] [ HH.div [ classes [ dFlex, justifyContentCenter, pb2 ] ] [ e ] ]
    mkNumberButton n =
      mkElement $ HH.button [ classes buttonClasses, onClick \_ -> ActionClicked (ButtonNumber (ButtonId n)) ]
        [ HH.text $ show n
        ]
  in
    HH.div [ classes [ dFlex, justifyContentCenter, cKeyboard ] ]
      [ HH.div [ classes [ cKeyboardCol ] ]
          [ HH.div [ classes [ row, rowCols3, justifyContentCenter ] ] $
              ( concat $ 0 .. 2 <#> (\i -> 1 .. 3 <#> (\j -> mkNumberButton (i * 3 + j)))
              ) <>
                ( [ mkElement $ HH.button [ classes buttonClasses, onClick \_ -> ActionClicked ButtonDelete ]
                      [ HH.i [ classes [ bi, biBackspaceFill ] ] []
                      ]
                  , mkNumberButton 0
                  , mkElement $ HH.button [ classes buttonClasses, onClick \_ -> ActionClicked ButtonMinus ]
                      [ HH.text $ getSymbol OpMinus
                      ]
                  ]
                )
          ]
      ]