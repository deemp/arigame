module Main where

import Halogen.Frameworks.Bootstrap hiding (show, bi)
import Halogen.Frameworks.BootstrapIcons
import Prelude

import Classes
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Data.Array (concat, dropEnd, intercalate, zip)
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
import Halogen.HTML (IProp)
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

-- handleInitialize

data Operand = OpA | OpB | OpC

derive instance Eq Operand
derive instance Ord Operand

data Operator = Plus | Minus | Multiply

derive instance Eq Operator
derive instance Ord Operator

instance HasSymbol Operator where
  getSymbol = case _ of
    Plus -> "+"
    Minus -> "–"
    Multiply -> "×"

data ComparisonOperator = Equal

instance HasSymbol ComparisonOperator where
  getSymbol = case _ of
    Equal -> "="

data Unknown = Unknown

instance HasSymbol Unknown where
  getSymbol = case _ of
    Unknown -> "?"

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

class HasSymbol a where
  getSymbol :: a -> String

instance HasSymbol Operand where
  getSymbol = case _ of
    OpA -> "A"
    OpB -> "Б"
    OpC -> "В"

instance HasSymbol Bound where
  getSymbol = case _ of
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
    operandInputId = getSymbol operand <> "-" <> getSymbol bound
  in
    HH.div [ classes [ col, pb0 ] ]
      [ HH.div [ classes [ formFloating, cBound, ps1, pe1 ] ]
          [ HH.input
              [ type_ InputNumber
              , pattern "^[-]0|[1-9][0-9]*$"
              , classes [ formControl ]
              , id operandInputId
              , placeholder $ show placeholder_
              , onValueChange \inp -> BoundChanged operand bound inp
              ]
          , HH.label
              [ for operandInputId ]
              [ HH.text $ getRuName bound ]
          ]
      ]

settingsClasses ∷ Array ClassName
settingsClasses = [ cExercise, btn ]

mkOperandButton ∷ forall m. Operand → H.ComponentHTML Action () m
mkOperandButton operand =
  HH.div [ classes [ col2, p1 ] ]
    [ HH.div [ classes [ dFlex, justifyContentCenter ] ]
        [ HH.button [ classes settingsClasses, onClick \_ -> ToggleOperand operand ]
            [ HH.text $ getSymbol operand
            ]
        ]
    ]

mkOperand ∷ Operand → State → forall m. H.ComponentHTML Action () m
mkOperand operand state =
  HH.div [ classes [ dFlex, pb2, alignItemsCenter ] ] $
    let
      mkBound bound = mkOperandBound operand bound (fromMaybe 0 (Map.lookup (operand /\ bound) state.problemState.operandBoundValuePlaceholder))
    in
      [ mkBound BoundMin
      , mkOperandButton operand
      , mkBound BoundMax
      ]

mkOperators :: forall m. H.ComponentHTML Action () m
mkOperators =
  HH.div [ classes [ dFlex, justifyContentCenter, pb2 ] ]
    [ HH.div [ classes [ col ] ]
        [ HH.div [ classes [ dFlex, justifyContentCenter ] ]
            ( zip (zip operators [ justifyContentEnd, justifyContentCenter, justifyContentStart ]) [ 1, 2, 3 ] <#>
                ( \((x /\ justify) /\ idx) ->
                    HH.div [ classes [ (if idx == 2 then col2 else col) ] ]
                      [ HH.div
                          [ classes [ dFlex, justify ] ]
                          [ HH.button [ classes settingsClasses, onClick \_ -> ToggleOperator x ]
                              [ HH.text $ getSymbol x
                              ]
                          ]
                      ]
                )
            )
        ]
    ]

mkOperatorComparison :: forall m. H.ComponentHTML Action () m
mkOperatorComparison =
  HH.div [ classes [ dFlex, justifyContentCenter ] ]
    [ HH.div [ classes [ row, justifyContentCenter ] ]
        [ HH.div [ classes [ btnGroup, pb2 ] ]
            [ HH.button ([ classes settingsClasses ])
                [ HH.text $ getSymbol Equal
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
        , cSettingsPane
        -- , ClassName "show"
        ]
    , tabIndex (-1)
    , id offcanvasBottomId
    , ariaLabelledby offcanvasBottomLabel
    ]
    ( let
        colWidths = [ cSettings ]
      in
        [ HH.div [ classes [ dFlex, justifyContentCenter ] ]
            [ HH.div [ classes ([ offcanvasHeader, ms1, me1, pb1 ] <> colWidths) ]
                [ HH.h2 [ classes [ offcanvasTitle ] ] [ HH.text "Настройки" ]
                , HH.button [ type_ ButtonButton, classes [ btnClose, textReset ], ariaLabel "Close", dataBsDismiss "offcanvas", onClick \_ -> ToggleSettings ] []
                ]
            ]
        , HH.div [ classes [ offcanvasBody, pt1 ], id settingsId ]
            [ HH.div [ classes [ dFlex, justifyContentCenter, pt2, pb2 ] ]
                [ HH.div [ classes colWidths ]
                    [ mkOperand OpA state
                    , mkOperators
                    , mkOperand OpB state
                    , mkOperatorComparison
                    , mkOperand OpC state
                    ]
                ]
            ]
        ]
    )

mkHeader :: forall m. State -> H.ComponentHTML Action () m
mkHeader state =
  HH.div [ classes [ dFlex, justifyContentCenter, p2, cHeader ] ]
    [ HH.div [ classes [ col ] ]
        [ HH.div [ classes [ dFlex, justifyContentCenter, alignItemsCenter ] ]
            ( [ HH.div [ classes [ col ] ]
                  [ HH.div [ classes [ dFlex, justifyContentEnd ] ]
                      [ HH.button
                          [ classes [ btn, p2, pt1, pb1, cSettings ]
                          , dataBsTarget offcanvasBottomId
                          , type_ ButtonButton
                          , dataBsToggle "offcanvas"
                          , ariaControls "offcanvasBottom"
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

mkExercise :: forall m. State -> H.ComponentHTML Action () m
mkExercise state = do
  let
    renderOperand :: Operand -> H.ComponentHTML Action () m
    renderOperand op = HH.span [ classes [ ps2, pe2 ] ] [ HH.text (maybe "?" show (Map.lookup op state.problemState.operandValue)) ]

    render_ :: forall a. HasSymbol a => String -> Lens' ProblemState (Maybe a) -> H.ComponentHTML Action () m
    render_ def l = HH.span [ classes [ ps2, pe2 ] ] [ HH.text (maybe def getSymbol (view l state.problemState)) ]
  HH.div [ classes [ dFlex, justifyContentCenter ] ]
    [ HH.div [ classes [ col, cExercise ] ]
        [ HH.div [ classes [ fwBolder, textCenter, cExercise ] ]
            [ HH.p_
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
              ( concat $ [ 0, 1, 2 ] <#> (\i -> [ 1, 2, 3 ] <#> (\j -> mkNumberButton (i * 3 + j)))
              ) <>
                ( [ mkElement $ HH.button [ classes buttonClasses, onClick \_ -> DeleteButtonClicked ]
                      [ HH.i [ classes [ bi, biBackspaceFill ] ] []
                      ]
                  , mkNumberButton 0
                  , mkElement $ HH.button [ classes buttonClasses, onClick \_ -> MinusButtonClicked ]
                      [ HH.text $ getSymbol Minus
                      ]
                  ]
                )
          ]
      ]

root :: forall m. State -> H.ComponentHTML Action () m
root state =
  HH.div [ classes [ col ] ]
    [ mkHeader state
    , mkExercise state
    , mkKeyboard
    , mkSettings state
    ]