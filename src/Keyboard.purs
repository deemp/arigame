module Keyboard where

import Prelude

import CSSFrameworks.Bootstrap (bi, btn, btnSecondary, col, dFlex, justifyContentCenter, m1, p0, p1, pb2, row, rowCols3)
import ClassNames (cButtonSquare, cKeyboard, cKeyboardCol)
import Actions (Action(..))
import CSSFrameworks.BootstrapIcons (biBackspaceFill)
import Classes (getSymbol)
import Data (ButtonId(..), Operator(..))
import Data.Array (concat, (..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)

mkKeyboard :: forall m. H.ComponentHTML Action () m
mkKeyboard =
  let
    buttonClasses = [ btn, btnSecondary, cButtonSquare, p1, m1 ]
    mkElement e = HH.div [ classes [ col, p0 ] ] [ HH.div [ classes [ dFlex, justifyContentCenter, pb2 ] ] [ e ] ]
    mkNumberButton n =
      mkElement $ HH.button [ classes buttonClasses, onClick \_ -> NumberButtonClicked (ButtonId n) ]
        [ HH.text $ show n
        ]
  in
    HH.div [ classes [ dFlex, justifyContentCenter, cKeyboard ] ]
      [ HH.div [ classes [ cKeyboardCol ] ]
          [ HH.div [ classes [ row, rowCols3, justifyContentCenter ] ] $
              ( concat $ 0 .. 2 <#> (\i -> 1 .. 3 <#> (\j -> mkNumberButton (i * 3 + j)))
              ) <>
                ( [ mkElement $ HH.button [ classes buttonClasses, onClick \_ -> DeleteButtonClicked ]
                      [ HH.i [ classes [ bi, biBackspaceFill ] ] []
                      ]
                  , mkNumberButton 0
                  , mkElement $ HH.button [ classes buttonClasses, onClick \_ -> MinusButtonClicked ]
                      [ HH.text $ getSymbol OpMinus
                      ]
                  ]
                )
          ]
      ]