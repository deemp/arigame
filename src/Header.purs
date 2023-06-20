module Header where

import Prelude
import CSSFrameworks.Bootstrap (alignItemsCenter, btn, col, dFlex, justifyContentCenter, justifyContentEnd, justifyContentStart, m0, p2, pb1, pe1, ps1, pt1)
import CSSFrameworks.BootstrapIcons (bi, biCheckCircleFill, biGearFill, biXCircleFill)
import IProps (aAriaControls, aDataBsTarget, aDataBsToggle)
import Settings (offcanvasBottomId)
import Actions (Action(..))
import ClassNames (cCorrect, cCounter, cHeader, cIncorrect, cSettings)
import Data.Lens (view)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (ButtonType(..), classes, type_)
import Utils (p)

type HeaderState =
  { counterAnswersCorrect :: Int
  , counterAnswersIncorrect :: Int
  }

mkHeader :: forall m. HeaderState -> H.ComponentHTML Action () m
mkHeader state =
  HH.div [ classes [ dFlex, justifyContentCenter, p2, cHeader ] ]
    [ HH.div [ classes [ col ] ]
        [ HH.div [ classes [ dFlex, justifyContentCenter, alignItemsCenter ] ]
            ( [ HH.div [ classes [ col ] ]
                  [ HH.div [ classes [ dFlex, justifyContentEnd ] ]
                      [ HH.button
                          [ classes [ btn, p2, pt1, pb1, cSettings ]
                          , aDataBsTarget offcanvasBottomId
                          , type_ ButtonButton
                          , aDataBsToggle "offcanvas"
                          , aAriaControls "offcanvasBottom"
                          , onClick \_ -> ToggleSettings
                          ]
                          [ HH.i [ classes [ bi, biGearFill, cSettings ] ] []
                          ]
                      ]
                  ]
              ] <>
                ( let
                    mkCounter cls icon lcounter justify =
                      HH.div [ classes [ col, cls ] ]
                        [ HH.div [ classes [ dFlex, justify ] ]
                            [ HH.i [ classes [ bi, icon, p2, pe1, cls ] ] []
                            , HH.p [ classes [ cCounter, p2, ps1, m0 ] ] [ HH.text (show (view lcounter state)) ]
                            ]
                        ]
                  in
                    [ mkCounter cCorrect biCheckCircleFill (p @"counterAnswersCorrect") justifyContentCenter
                    , mkCounter cIncorrect biXCircleFill (p @"counterAnswersIncorrect") justifyContentStart
                    ]
                )
            )
        ]
    ]