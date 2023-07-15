module Header where

import Prelude

import CSSFrameworks.Bootstrap (alignItemsCenter, btn, col, dFlex, justifyContentCenter, justifyContentEnd, justifyContentStart, m0, p2, pb1, pe1, ps1, pt1)
import CSSFrameworks.BootstrapIcons (bi, biCheckCircleFill, biGearFill, biXCircleFill)
import ClassNames (cCorrect, cCounter, cHeader, cIncorrect, cSettings)
import Data.Lens (over, view, (%~))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (ButtonType(..), classes, type_)
import IProps (aAriaControls, aDataBsTarget, aDataBsToggle)
import Settings (offcanvasBottomId)
import Utils (bw)

type State =
  { correct :: Int
  , incorrect :: Int
  }

defaultState :: State
defaultState =
  { correct: 0
  , incorrect: 0
  }

data Action = ActionToggleSettings

type Slot id = forall output. H.Slot Query output id

data Query (a :: Type) = QueryIncrementCorrect Boolean a

component :: forall input output m. MonadAff m => H.Component Query input output m
component = H.mkComponent
  { initialState: const defaultState
  , render: render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      }
  }
  where
  handleQuery :: forall a. Query a -> H.HalogenM State Action () output m (Maybe a)
  handleQuery = case _ of
    QueryIncrementCorrect c reply ->
      H.modify_ (over (if c then bw @"correct" else bw @"incorrect") %~ (_ + 1)) $> Just reply

render :: forall m. State -> H.ComponentHTML Action () m
render state =
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
                          , onClick \_ -> ActionToggleSettings
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
                    [ mkCounter cCorrect biCheckCircleFill (bw @"correct") justifyContentCenter
                    , mkCounter cIncorrect biXCircleFill (bw @"incorrect") justifyContentStart
                    ]
                )
            )
        ]
    ]