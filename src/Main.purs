module Main where

import Prelude

import CSSFrameworks.Bootstrap (alignItemsCenter, btn, btnClose, btnDanger, btnGroup, btnSecondary, col, col2, dFlex, formControl, formFloating, fwBolder, h75, isInvalid, justifyContentCenter, justifyContentEnd, justifyContentStart, m0, m1, me1, ms1, offcanvas, offcanvasBody, offcanvasBottom, offcanvasHeader, offcanvasTitle, p0, p1, p2, pb0, pb1, pb2, pe1, pe2, ps1, ps2, pt1, pt2, row, rowCols3, textCenter, textReset)
import CSSFrameworks.BootstrapIcons (bi, biBackspaceFill, biCheckCircleFill, biGearFill, biXCircleFill)
import Classes (cBound, cButtonSquare, cCorrect, cCounter, cError, cExercise, cHeader, cIncorrect, cKeyboard, cKeyboardCol, cSelect, cSelected, cSettings, cSettingsPane)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Data.Array (concat, filter, head, intercalate, zip)
import Data.Either (either)
import Data.Int (fromString)
import Data.Lens (Lens', over, view, (%~), (.~))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Set (Set, member, size)
import Data.Set as Set
import Data.String (length, take)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueChange, onValueInput)
import Halogen.HTML.Properties (class_, classes, for, id, placeholder, tabIndex, type_, value)
import Halogen.VDom.Driver (runUI)
import IProps (aAriaControls, aAriaLabel, aDataBsDismiss, aDataBsTarget, aDataBsToggle, aDisabled)
import Unsafe.Coerce (unsafeCoerce)
import Utils (p, singletonIf)

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

data Operator = OpPlus | OpMinus | OpMultiply

derive instance Eq Operator
derive instance Ord Operator

instance HasSymbol Operator where
  getSymbol = case _ of
    OpPlus -> "+"
    OpMinus -> "–"
    OpMultiply -> "×"

data OperatorComparison = OpEqual

derive instance Eq OperatorComparison
derive instance Ord OperatorComparison

instance HasSymbol OperatorComparison where
  getSymbol = case _ of
    OpEqual -> "="

data Unknown = Unknown

instance HasSymbol Unknown where
  getSymbol = case _ of
    Unknown -> "?"

data Bound = BoundMin | BoundMax

derive instance Eq Bound
derive instance Ord Bound

operatorsList :: Array Operator
operatorsList = [ OpPlus, OpMinus, OpMultiply ]

type ExerciseState =
  { operandValue :: Map Operand Int
  , answerCurrent :: String
  -- empty string means unknown answer
  , answerCorrect :: Maybe Int
  , operatorCurrent :: Maybe Operator
  , operatorComparisonCurrent :: Maybe OperatorComparison
  }

type SettingsState =
  { isOpen :: Boolean
  -- avoid capturing digit input when settings is open
  , operandsSelected :: Set Operand
  , operatorsSelected :: Set Operator
  , operandBoundValue :: Map (Operand /\ Bound) String
  , operandBoundValuePlaceholder :: Map (Operand /\ Bound) String
  , operatorsComparisonSelected :: Set OperatorComparison
  , targetOperand :: Maybe Operand
  , buttonErrors :: Set SettingsButtonError
  }

data SettingsButtonError
  = NoOperandSelected
  | NoOperatorSelected
  | NoOperatorComparisonSelected

derive instance Eq SettingsButtonError
derive instance Ord SettingsButtonError

type HeaderState =
  { counterAnswersCorrect :: Int
  , counterAnswersIncorrect :: Int
  }

type State =
  { headerState :: HeaderState
  , exerciseState :: ExerciseState
  , settingsState :: SettingsState
  }

initialState :: State
initialState =
  { headerState:
      { counterAnswersCorrect: 0
      , counterAnswersIncorrect: 0
      }
  , exerciseState:
      { operandValue: Map.empty
      , answerCurrent: ""
      , answerCorrect: Nothing
      , operatorCurrent: Nothing
      , operatorComparisonCurrent: Nothing
      }
  , settingsState:
      { isOpen: false
      , operandsSelected: Set.empty
      , operatorsSelected: Set.empty
      , operandBoundValue: Map.empty
      , operandBoundValuePlaceholder: Map.empty
      , operatorsComparisonSelected: Set.empty
      , targetOperand: Nothing
      , buttonErrors: Set.empty
      }
  }

type St = { someField :: Int }

st :: St
st = { someField: 1 }

operandsList ∷ Array Operand
operandsList = [ OpA, OpB, OpC ]

fixNumber :: String -> String
fixNumber s = either (const "bad") identity res
  where
  res = do
    let replace_ reg repl s_ = regex reg global >>= \x -> pure $ replace x repl s_
    s1 <- replace_ "[^0-9-]" "" s
    s2 <- replace_ "(\\d)-+" "$1" s1
    s3 <- replace_ "^-+" "-" s2
    s4 <- replace_ "^(-?)0+([1-9])" "$1$2" s3
    s5 <- replace_ "^-0+$" "-" s4
    s6 <- replace_ "^0+$" "0" s5
    pure s6

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction =
  case _ of
    NumberButtonClicked (ButtonId bid) ->
      modifyAnswerCurrent (_ <> show bid)
    MinusButtonClicked -> do
      modifyAnswerCurrent fixNumber
    DeleteButtonClicked ->
      modifyAnswerCurrent (\x -> take (max (length x) 0) x)
    ToggleOperator op -> do
      toggleSelected (p @"operatorsSelected") op
      toggleNoSelectedError (p @"operatorsSelected") NoOperatorSelected
    ToggleOperatorComparison op -> do
      toggleSelected (p @"operatorsComparisonSelected") op
      toggleNoSelectedError (p @"operatorsComparisonSelected") NoOperatorComparisonSelected
    ToggleOperand op -> do
      toggleSelected (p @"operandsSelected") op

      -- if 2 operands selected, set target operand
      operands <- H.gets _.settingsState.operandsSelected
      let
        operand =
          if size operands == 2 then
            -- find a non-selected operator
            head (filter (\x -> not (member x operands)) operandsList)
          else Nothing
      H.modify_ (over (p @"settingsState" <<< p @"targetOperand") .~ operand)

      toggleNoSelectedError (p @"operandsSelected") NoOperandSelected
    ToggleSettings -> H.modify_ (over (p @"settingsState" <<< p @"isOpen") %~ not)
    BoundChanged op bound inp -> do
      let inp_ = fixNumber inp
      H.modify_ (over (p @"settingsState" <<< p @"operandBoundValue") %~ Map.insert (op /\ bound) (fixNumber inp))

      liftEffect $ log inp_

      -- ugly hack to re-render
      when (fixNumber inp /= inp)
        ( do
            handleAction $ BoundChanged op bound (inp_ <> "9")
            handleAction $ BoundChanged op bound inp_
        )
    Init -> do
      handleAction $ ToggleOperand OpA
      handleAction $ ToggleOperand OpB
      handleAction $ ToggleOperator OpMinus
      handleAction $ ToggleOperatorComparison OpEqual

      handleAction $ BoundChanged OpA BoundMin "10"
      handleAction $ BoundChanged OpA BoundMax "20"
      handleAction $ BoundChanged OpB BoundMin "0"
      handleAction $ BoundChanged OpB BoundMax "10"

  where
  toggleNoSelectedError :: forall output' m' a. MonadEffect m' => Lens' SettingsState (Set a) -> SettingsButtonError -> H.HalogenM State Action () output' m' Unit
  toggleNoSelectedError l err = do
    os <- H.gets (view (p @"settingsState" <<< l))
    H.modify_
      ( over (p @"settingsState" <<< p @"buttonErrors") %~
          if
            Set.isEmpty os then Set.insert err
          else Set.delete err
      )

  toggleSelected :: forall output' m' a. Ord a => MonadEffect m' => Lens' SettingsState (Set a) -> a -> H.HalogenM State Action () output' m' Unit
  toggleSelected l op = H.modify_ (over (p @"settingsState" <<< l) %~ Set.toggle op)

  modifyAnswerCurrent :: (String -> String) -> H.HalogenM State Action () output m Unit
  modifyAnswerCurrent f = H.modify_ $ over (p @"exerciseState" <<< p @"answerCurrent") %~ f

data Action
  = NumberButtonClicked ButtonId
  | MinusButtonClicked
  | DeleteButtonClicked
  | ToggleOperator Operator
  | ToggleOperatorComparison OperatorComparison
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

hasErrorBound :: SettingsState -> Operand -> Bound -> Boolean
hasErrorBound state operand bound = (length b == 0) || not (b1 <= b2)
  where
  b = fromMaybe "" (Map.lookup (operand /\ bound) state.operandBoundValue)
  b1 = fromString $ fromMaybe "0" $ Map.lookup (operand /\ BoundMin) state.operandBoundValue
  b2 = fromString $ fromMaybe "0" $ Map.lookup (operand /\ BoundMax) state.operandBoundValue

mkOperandBound ∷ forall m. SettingsState -> Operand → Bound -> H.ComponentHTML Action () m
mkOperandBound state operand bound =
  let
    operandInputId = getSymbol operand <> "-" <> getSymbol bound
    placeholder_ = fromMaybe "" (Map.lookup (operand /\ bound) state.operandBoundValuePlaceholder)
    value_ = fromMaybe "" (Map.lookup (operand /\ bound) state.operandBoundValue)
    isDisabled = state.targetOperand == Just operand
    hasError = hasErrorBound state operand bound
  in
    HH.div [ classes [ col, pb0 ] ]
      [ HH.div [ classes [ formFloating, cBound, ps1, pe1 ] ]
          [ HH.input $
              [ classes $ [ formControl ] <> singletonIf hasError isInvalid
              , id operandInputId
              , placeholder placeholder_
              , value value_
              , onValueChange \inp -> BoundChanged operand bound inp
              , onValueInput \inp -> BoundChanged operand bound inp
              ] <> singletonIf isDisabled aDisabled
          , HH.label
              [ for operandInputId ]
              [ HH.text $ getRuName bound ]
          ]
      ]

settingsButtonClasses ∷ Boolean -> Boolean -> Array ClassName
settingsButtonClasses isSelected isError = [ btn, cSelect ] <>
  if isError then [ cError ]
  else singletonIf isSelected cSelected

mkOperandButton ∷ forall m. SettingsState -> Operand → H.ComponentHTML Action () m
mkOperandButton state operand =
  HH.div [ classes [ col2, p1 ] ]
    [ HH.div [ classes [ dFlex, justifyContentCenter ] ]
        [ let
            isDisabled = (state.targetOperand == Just operand)
          in
            HH.button
              ( [ classes $
                    ( settingsButtonClasses
                        ( member operand state.operandsSelected
                            || Just operand == state.targetOperand
                        )
                        (Set.member NoOperandSelected state.buttonErrors)
                    )
                , onClick \_ -> ToggleOperand operand
                ]
                  <> singletonIf isDisabled aDisabled
              )
              [ HH.text $ getSymbol operand
              ]
        ]
    ]

mkOperand ∷ SettingsState → Operand → forall m. H.ComponentHTML Action () m
mkOperand state operand =
  HH.div [ classes [ dFlex, pb2, alignItemsCenter ] ] $
    let
      mkBound bound = mkOperandBound state operand bound
    in
      [ mkBound BoundMin
      , mkOperandButton state operand
      , mkBound BoundMax
      ]

mkOperators :: forall m. SettingsState -> H.ComponentHTML Action () m
mkOperators state =
  HH.div [ classes [ dFlex, justifyContentCenter, pb2 ] ]
    [ HH.div [ classes [ col ] ]
        [ HH.div [ classes [ dFlex, justifyContentCenter ] ]
            ( zip (zip operatorsList [ justifyContentEnd, justifyContentCenter, justifyContentStart ]) [ 1, 2, 3 ] <#>
                ( \((x /\ justify) /\ idx) ->
                    HH.div [ classes [ (if idx == 2 then col2 else col) ] ]
                      [ HH.div
                          [ classes [ dFlex, justify ] ]
                          [ HH.button
                              [ classes
                                  ( settingsButtonClasses
                                      (member x state.operatorsSelected)
                                      (Set.member NoOperatorSelected state.buttonErrors)
                                  )
                              , onClick \_ -> ToggleOperator x
                              ]
                              [ HH.text $ getSymbol x
                              ]
                          ]
                      ]
                )
            )
        ]
    ]

mkOperatorComparison :: forall m. SettingsState -> H.ComponentHTML Action () m
mkOperatorComparison state =
  let
    operator = OpEqual
  in
    HH.div [ classes [ dFlex, justifyContentCenter ] ]
      [ HH.div [ classes [ row, justifyContentCenter ] ]
          [ HH.div [ classes [ btnGroup, pb2 ] ]
              [ let
                  isSelected = member operator state.operatorsComparisonSelected
                in
                  HH.button
                    ( [ classes (settingsButtonClasses isSelected (Set.member NoOperatorComparisonSelected state.buttonErrors))
                      , onClick \_ -> ToggleOperatorComparison operator
                      ]
                    )
                    [ HH.text $ getSymbol operator
                    ]
              ]
          ]
      ]

offcanvasBottomId :: String
offcanvasBottomId = "offcanvas-bottom"

mkSettings :: forall m. SettingsState -> H.ComponentHTML Action () m
mkSettings state =
  HH.div
    [ classes $
        [ offcanvas
        , offcanvasBottom
        , h75
        , cSettingsPane
        ]
    , tabIndex (-1)
    , id offcanvasBottomId
    ]
    ( let
        colWidths = [ cSettings ]
      in
        [ HH.div [ classes [ dFlex, justifyContentCenter ] ]
            [ HH.div [ classes ([ offcanvasHeader, ms1, me1, pb1 ] <> colWidths) ]
                [ HH.h2 [ classes [ offcanvasTitle ] ] [ HH.text "Настройки" ]
                , HH.button
                    [ type_ ButtonButton
                    , classes [ btnClose, textReset ]
                    , aAriaLabel "Close"
                    , aDataBsDismiss "offcanvas"
                    , onClick \_ -> ToggleSettings
                    ]
                    []
                ]
            ]
        , HH.div [ classes [ offcanvasBody, pt1 ] ]
            [ HH.div [ classes [ dFlex, justifyContentCenter, pt2, pb2 ] ]
                [ HH.div [ classes colWidths ]
                    ( let
                        mkF :: forall a. (SettingsState -> a) -> a
                        mkF f = f state
                      in
                        [ mkF mkOperand OpA
                        , mkF mkOperators
                        , mkF mkOperand OpB
                        , mkF mkOperatorComparison
                        , mkF mkOperand OpC
                        ]
                    )
                ]
            ]
        ]
    )

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

mkExercise :: forall m. ExerciseState -> H.ComponentHTML Action () m
mkExercise state = do
  let
    renderOperand :: Operand -> H.ComponentHTML Action () m
    renderOperand op = HH.span [ classes [ ps2, pe2 ] ] [ HH.text (maybe "?" show (Map.lookup op state.operandValue)) ]

    render_ :: forall a. HasSymbol a => String -> Lens' ExerciseState (Maybe a) -> H.ComponentHTML Action () m
    render_ def l = HH.span [ classes [ ps2, pe2 ] ] [ HH.text (maybe def getSymbol (view l state)) ]
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
                      [ HH.text $ getSymbol OpMinus
                      ]
                  ]
                )
          ]
      ]

root :: forall m. State -> H.ComponentHTML Action () m
root state =
  HH.div [ classes [ col ] ]
    [ mkHeader state.headerState
    , mkExercise state.exerciseState
    , mkKeyboard
    , mkSettings state.settingsState
    ]