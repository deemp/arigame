module Main where

import Prelude

import Classes (cButtonSquare, cCorrect, cExercise, cIcon, cIncorrect, cNohover, cSettings)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Data.Array (dropEnd, intercalate, zip)
import Data.Lens (Lens', _Just, over, view, (%~), (<>~))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Exception (throw)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Frameworks.Bootstrap (bi, btn, btnClose, btnGroup, btnSecondary, col, col10, col2, col4, col5, col8, dFlex, dGrid, formControl, formFloating, fs2, fwBolder, h75, justifyContentCenter, justifyContentEnd, justifyContentLgEnd, justifyContentLgStart, justifyContentStart, mb2, me1, ms1, mt2, offcanvas, offcanvasBody, offcanvasBottom, offcanvasHeader, offcanvasTitle, p0, p1, pb2, pe0, pe2, ps0, ps2, pt2, row, textCenter, textReset)
import Halogen.Frameworks.BootstrapIcons (biCheckCircleFill, biDash, biGearFill, biPlus, biX)
import Halogen.Frameworks.TablerIcons (ti, tiEqual, tiQuestionMark)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (InputType(..), class_, classes, for, id, pattern, placeholder, tabIndex, type_)
import Halogen.VDom.Driver (runUI)
import Lens (p)
import Props (ariaControls, ariaLabel, ariaLabelledby, dataBsDismiss, dataBsTarget, dataBsToggle)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState: const initialState
    , render: root
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

-- handleInitializez

data Operand = OpA | OpB | OpC

derive instance Eq Operand
derive instance Ord Operand

data Operator = Plus | Minus | Multiply

derive instance Eq Operator
derive instance Ord Operator

instance Show Operator where
  show = case _ of
    Plus -> "+"
    Minus -> "-"
    Multiply -> "×"

data ComparisonOperator = Equal

instance Show ComparisonOperator where
  show = case _ of
    Equal -> "="

class HasIcon a where
  getIcon :: forall b c. a -> HTML b c
  getIcon' :: forall b i. a -> Array ClassName -> HTML b i

instance HasIcon Operator where
  getIcon' x r = HH.i
    ( [ classes $
          [ ti
          , case x of
              Plus -> biPlus
              Minus -> biDash
              Multiply -> biX
          ] <> r
      ]
    )
    []
  getIcon x = getIcon' x []

data Unknown = Unknown

instance HasIcon Unknown where
  getIcon' _ r = HH.i ([ classes $ [ ti, tiQuestionMark ] <> r ]) []
  getIcon x = getIcon' x []

instance HasIcon a => HasIcon (Maybe a) where
  getIcon' x r = maybe (getIcon' Unknown r) (\y -> getIcon' y r) x
  getIcon x = getIcon' x []

instance HasIcon ComparisonOperator where
  getIcon' x r = case x of
    Equal -> HH.i ([ classes $ [ ti, tiEqual ] <> r ]) []
  getIcon x = getIcon' x []

data Bound = BoundMin | BoundMax

derive instance Eq Bound
derive instance Ord Bound

operators :: Array Operator
operators = [ Plus, Minus, Multiply ]

type AnswerCurrent =
  { hasMinus :: Boolean
  , number :: String
  }

type ProblemState =
  { operandValue :: Map Operand Int
  , operandBoundValue :: Map (Operand /\ Bound) Int
  , operandBoundValuePlaceholder :: Map (Operand /\ Bound) Int
  -- Nothing ~ `?`
  , answerCurrent :: Maybe AnswerCurrent
  , answerCorrect :: Maybe Int
  , operatorCurrent :: Maybe Operator
  , operatorComparisonCurrent :: Maybe ComparisonOperator
  }

type Placeholder =
  { boundMinPlaceholder :: Int
  , boundMaxPlaceholder :: Int
  }

type State =
  { counterAnswersCorrect :: Int
  , counterAnswersIncorrect :: Int
  , isSettingsOpen :: Boolean
  , operandsSelected :: Set Operand
  , operatorsSelected :: Set Operator
  , isKeyboardDisabled :: Boolean
  , problemState :: ProblemState
  }

initialState :: State
initialState =
  { counterAnswersCorrect: 0
  , counterAnswersIncorrect: 0
  , isSettingsOpen: false
  , operandsSelected: Set.empty
  , operatorsSelected: Set.empty
  , isKeyboardDisabled: false
  , problemState:
      { operandValue: Map.empty
      , operandBoundValue: Map.empty
      , operandBoundValuePlaceholder: Map.empty
      , answerCurrent: Nothing
      , answerCorrect: Nothing
      , operatorCurrent: Nothing
      , operatorComparisonCurrent: Nothing
      }
  }

type St = { someField :: Int }

st :: St
st = { someField: 1 }

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  -- TODO modify current operand
  NumberButtonClicked bid -> H.modify_ (over (p @"problemState" <<< p @"answerCurrent" <<< _Just <<< p @"number") <>~ show bid)
  MinusButtonClicked -> H.modify_ (over (p @"problemState" <<< p @"answerCurrent" <<< _Just <<< p @"hasMinus") %~ not)
  DeleteButtonClicked -> H.modify_ $
    over
      (p @"problemState" <<< p @"answerCurrent")
      (_ >>= (\z -> if z.number == "" then Nothing else Just z { number = (fromCharArray (dropEnd 1 (toCharArray z.number))) }))
  ToggleOperator op -> H.modify_ (over (p @"operatorsSelected") %~ Set.toggle op)
  ToggleSettings -> H.modify_ (over (p @"isSettingsOpen") %~ not)
  ToggleOperand op -> H.modify_ (over (p @"operandsSelected") %~ Set.toggle op)
  BoundChanged op bound inp ->
    -- TODO
    H.modify_ identity
  Init ->
    -- TODO calculate the missing argument from existing
    H.modify_ identity

-- TODO
-- easy/hard
-- +, -, *
-- hard - negative numbers, <=, /

-- TODO split into components
-- root
-- - calculator
-- - settings

data Action
  = NumberButtonClicked ButtonId
  | MinusButtonClicked
  | DeleteButtonClicked
  | ToggleOperator Operator
  | ToggleOperand Operand
  | ToggleSettings
  | BoundChanged Operand Bound String
  | Init

newtype ButtonId = ButtonId Int

derive newtype instance Show ButtonId

classes_ :: forall (t1 :: Row Type) (t2 :: Type). Array String -> Array (IProp (class :: String | t1) t2)
classes_ a = [ class_ $ ClassName $ intercalate " " a ]

error :: ∀ a. String -> a
error = unsafeCoerce <<< throw

undefined :: forall a. a
undefined = error "undefined"

class HasName a where
  getName :: a -> String

instance HasName Operand where
  getName = case _ of
    OpA -> "a"
    OpB -> "b"
    OpC -> "c"

instance HasName Bound where
  getName = case _ of
    BoundMin -> "min"
    BoundMax -> "max"

class HasRuName a where
  getRuName :: a -> String

instance HasRuName Bound where
  getRuName = case _ of
    BoundMin -> "Мин"
    BoundMax -> "Макс"

mkOperandBound ∷ forall m. Operand → Bound -> Int -> H.ComponentHTML Action () m
mkOperandBound operand bound placeholder_ =
  let
    operandInputId = getName operand <> "-" <> getName bound
  in
    HH.div [ classes [ col5, textCenter ] ]
      [ HH.div [ classes [ formFloating, pb2 ] ]
          [ HH.input
              [ type_ InputNumber
              , pattern "^[-]0|[1-9][0-9]*$"
              , classes [ formControl, ClassName "bound-input" ]
              , id operandInputId
              , placeholder $ show placeholder_
              , onValueChange \inp -> BoundChanged operand bound inp
              ]
          , HH.label
              [ for operandInputId ]
              [ HH.text $ getRuName bound ]
          ]
      ]

mkOperandButton ∷ forall m. Operand → H.ComponentHTML Action () m
mkOperandButton operand =
  HH.div [ classes [ col2, textCenter, p1 ] ]
    [ HH.div [ classes [ row, justifyContentCenter ] ]
        [ HH.button [ classes [ cNohover ], onClick \_ -> ToggleOperand operand ]
            [ HH.text $ getName operand
            ]
        ]
    ]

mkOperand ∷ Operand → State → forall m. H.ComponentHTML Action () m
mkOperand operand state =
  HH.div [ classes [ dFlex ] ] $
    let
      mkBound bound = mkOperandBound operand bound (fromMaybe 0 (Map.lookup (operand /\ bound) state.problemState.operandBoundValuePlaceholder))
    in
      [ mkBound BoundMin
      , mkOperandButton operand
      , mkBound BoundMax
      ]

mkOperators :: forall m. H.ComponentHTML Action () m
mkOperators =
  HH.div [ classes [ dFlex, justifyContentCenter ] ]
    ( zip operators [ justifyContentLgEnd, justifyContentCenter, justifyContentLgStart ] <#>
        ( \(x /\ justify) ->
            HH.div [ classes [ col2, pb2, justify ] ]
              [ HH.div
                  [ classes [ dFlex, justify ] ]
                  [ HH.button ([ classes [ cNohover ] ] <> [ onClick \_ -> ToggleOperator x ])
                      [ getIcon x
                      ]
                  ]
              ]
        )
    )

mkComparisonOperator :: forall m. H.ComponentHTML Action () m
mkComparisonOperator =
  HH.div [ classes [ dFlex, justifyContentCenter ] ]
    [ HH.div [ classes [ row, justifyContentCenter ] ]
        [ HH.div [ classes [ btnGroup, pb2 ] ]
            [ HH.button ([ classes [ cNohover ] ])
                [ getIcon Equal
                ]
            ]
        ]
    ]

offcanvasBottomId :: String
offcanvasBottomId = "offcanvas-bottom"

settingsId :: String
settingsId = "settings-body"

offcanvasBottomLabel ∷ String
offcanvasBottomLabel = "offcanvasBottomLabel"

mkSettings :: forall m. State -> H.ComponentHTML Action () m
mkSettings state =
  HH.div
    [ classes
        [ offcanvas
        , offcanvasBottom
        , h75
        , ClassName "show"
        ]
    , tabIndex (-1)
    , id offcanvasBottomId
    , ariaLabelledby offcanvasBottomLabel
    ]
    [ HH.div [ classes [ dFlex, justifyContentCenter ] ]
        [ HH.div [ classes [ col8 ] ]
            [ HH.div [ classes [ dFlex, justifyContentCenter ] ]
                [ HH.div [ classes [ offcanvasHeader, col, cSettings, ms1, me1 ] ]
                    [ HH.h5 [ classes [ offcanvasTitle ] ] [ HH.text "Настройки" ]
                    , HH.button [ type_ ButtonButton, classes [ btnClose, textReset ], ariaLabel "Close", dataBsDismiss "offcanvas", onClick \_ -> ToggleSettings ] []
                    ]
                ]
            , HH.div [ classes [ offcanvasBody ], id settingsId ]
                [ HH.div [ classes [ dFlex, justifyContentCenter, pt2, pb2 ] ]
                    [ HH.div [ classes [ col, cSettings ] ]
                        [ mkOperand OpA state
                        , mkOperators
                        , mkOperand OpB state
                        , mkComparisonOperator
                        , mkOperand OpC state
                        ]
                    ]
                ]

            ]
        ]
    ]

mkHeader :: forall m. State -> H.ComponentHTML Action () m
mkHeader state =
  HH.div [ classes [ dFlex, justifyContentCenter, pt2, pb2 ] ] $
    [ HH.div [ classes [ col2, cIcon ] ]
        [ HH.div [ classes [ dFlex, justifyContentEnd ] ]
            [ HH.button [ classes [ btn, p0 ], dataBsTarget offcanvasBottomId, type_ ButtonButton, dataBsToggle "offcanvas", ariaControls "offcanvasBottom", onClick \_ -> ToggleSettings ]
                [ HH.i [ classes [ bi, biGearFill, fs2 ] ] []
                ]
            ]
        ]
    ] <>
      ( let
          mkCounter cls lcounter justify =
            HH.div [ classes [ col2, cIcon ] ]
              [ HH.div [ classes [ dFlex, justify ] ]
                  [ HH.i [ classes [ bi, biCheckCircleFill, pe2, cls, fs2 ] ] []
                  , HH.span [ classes [ ClassName "exercise-counter", p0 ] ] [ HH.text (show (view lcounter state)) ]
                  ]
              ]
        in
          [ mkCounter cCorrect (p @"counterAnswersCorrect") justifyContentCenter
          , mkCounter cIncorrect (p @"counterAnswersIncorrect") justifyContentStart
          ]
      )

mkExercise :: forall m. State -> H.ComponentHTML Action () m
mkExercise state = do
  let
    renderOperand :: Operand -> H.ComponentHTML Action () m
    renderOperand op = HH.span [ classes [ ps2, pe2 ] ] [ HH.text (maybe "?" show (Map.lookup op state.problemState.operandValue)) ]

    render_ :: forall a. Show a => String -> Lens' ProblemState (Maybe a) -> H.ComponentHTML Action () m
    render_ def l = HH.span [ classes [ ps2, pe2 ] ] [ HH.text (maybe def show (view l state.problemState)) ]
  HH.div [ classes [ row, justifyContentCenter, pt2, pb2 ] ]
    [ HH.div [ classes [ col10 ] ]
        [ HH.div [ classes [ fwBolder, textCenter, cExercise ] ]
            [ HH.p [ classes [ mt2, mb2 ] ]
                [ renderOperand OpA
                , render_ "+" (p @"operatorCurrent")
                , renderOperand OpB
                , render_ "=" (p @"operatorComparisonCurrent")
                , renderOperand OpC
                ]
            ]
        ]
    ]

mkKeyboard :: forall m. H.ComponentHTML Action () m
mkKeyboard =
  HH.div [ classes [ row, justifyContentCenter ] ]
    [ HH.div [ classes [ col4 ] ]
        ( [ 0, 1, 2 ] <#>
            ( \i -> HH.div [ classes [ dFlex, justifyContentCenter, pt2, pb2 ] ]
                ( [ 1, 2, 3 ] <#>
                    ( \j -> HH.div [ classes [ col2, dGrid, justifyContentCenter ] ]
                        [ let
                            n = i * 3 + j
                          in
                            HH.button [ classes [ btn, btnSecondary, cButtonSquare, ps0, pe0 ], onClick \_ -> NumberButtonClicked (ButtonId n) ]
                              [ HH.text $ show n
                              ]
                        ]
                    )
                )
            )
        )
    ]

root :: forall m. State -> H.ComponentHTML Action () m
root state =
  HH.div [ classes [ col ] ]
    [ mkHeader state
    , mkExercise state
    , mkKeyboard
    , mkSettings state
    ]