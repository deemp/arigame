module Settings where

import Prelude

import Data (Bound(..), Operand(..), Operator(..), OperatorComparison(..), getOtherOperands, operatorsAll)
import Actions (Action(..))
import CSSFrameworks.Bootstrap (alignItemsCenter, btn, btnClose, btnGroup, col, col2, dFlex, formControl, formFloating, h75, isInvalid, justifyContentCenter, justifyContentEnd, justifyContentStart, me1, ms1, offcanvas, offcanvasBody, offcanvasBottom, offcanvasHeader, offcanvasTitle, p1, pb0, pb1, pb2, pe1, ps1, pt1, pt2, row, textReset)
import ClassNames (cBound, cError, cSelect, cSelected, cSettings, cSettingsPanel)
import Classes (getSymbol)
import Data.Array (concat, elem, zip)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (maximum, minimum)
import Data.Int (fromString)
import Data.Lens ((%~))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set, member)
import Data.Set as Set
import Data.String (length)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen (ClassName)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (ButtonType(..), classes, for, id, placeholder, tabIndex, type_, value)
import IProps (aAriaLabel, aDataBsDismiss, aDisabled, aType)
import Record.Format (format)
import Utils (error, p, singletonIf)

offcanvasBottomId :: String
offcanvasBottomId = "offcanvas-bottom"

type SettingsState =
  { isOpen :: Boolean
  -- avoid capturing digit input when settings is open
  , operandsSelected :: Set Operand
  , operatorsSelected :: Set Operator
  , operandBoundValue :: Map (Operand /\ Bound) String
  , operandBoundValuePlaceholder :: Map (Operand /\ Bound) String
  , operatorsComparisonSelected :: Set OperatorComparison
  , operandTarget :: Maybe Operand
  , buttonErrors :: Set SettingsError
  }

getOtherOperandsBounds :: SettingsState -> Operand -> Either SettingsError OtherOperandsBounds
getOtherOperandsBounds state operand = operandsBounds
  where
  (op1 /\ op2) = getOtherOperands operand

  operandBound op bound = maybe (Left $ NoBoundSet op bound) (Right) (fromString =<< Map.lookup (op /\ bound) state.operandBoundValue)
  operandBounds op = do
    opMin <- operandBound op BoundMin
    opMax <- operandBound op BoundMax
    when (opMin > opMax) (Left (BoundMinGreaterThanBoundMax op))
    pure (opMin /\ opMax)

  operandsBounds = do
    when (Set.isEmpty state.operatorsSelected) (Left NoOperatorSelected)
    when (Set.isEmpty state.operatorsComparisonSelected) (Left NoOperatorComparisonSelected)
    (op1Min /\ op1Max) <- operandBounds op1
    (op2Min /\ op2Max) <- operandBounds op2
    pure { op1Min, op1Max, op2Min, op2Max }

updateTargetOperandBounds :: SettingsState -> SettingsState
updateTargetOperandBounds state = do
  case state.operandTarget of
    Nothing -> state
    Just op ->
      let
        modifyBounds operators =
          let
            modifyVal mod_ = state # p @"operandBoundValue" %~ mod_
          in
            case getOtherOperandsBounds state op of
              Left _ -> modifyVal (Map.delete (op /\ BoundMin) <<< Map.delete (op /\ BoundMax))
              Right bounds -> do
                let (mini /\ maxi) = calculateBounds (Array.fromFoldable operators) bounds
                modifyVal (Map.insert (op /\ BoundMin) (show mini) <<< Map.insert (op /\ BoundMax) (show maxi))
      in
        modifyBounds (flipOperators state op)

type OtherOperandsBounds = { op1Min :: Int, op1Max :: Int, op2Min :: Int, op2Max :: Int }

getOperandTarget :: SettingsState -> Either SettingsError Operand
getOperandTarget state
  | Set.isEmpty state.operandsSelected = Left NoOperandSelected
  | Set.size state.operandsSelected == 1 =
      case Set.findMax state.operandsSelected of
        Nothing -> error "no max in set of operands"
        Just op -> Left $ OnlyOneOperandSelected op
  | otherwise =
      case state.operandTarget of
        Nothing -> error "Target operand not selected, but set size is 2"
        Just operandTarget -> Right operandTarget

data SettingsError
  = NoOperandSelected
  | NoOperatorSelected
  | NoOperatorComparisonSelected
  | NoBoundSet Operand Bound
  | BoundMinGreaterThanBoundMax Operand
  | OnlyOneOperandSelected Operand
  | NoExerciseGenerated

derive instance Eq SettingsError
derive instance Ord SettingsError

renderError :: SettingsError -> String
renderError e =
  case e of
    NoOperandSelected -> "Нажми на 'A' и 'Б'"
    NoOperatorSelected -> "Нажми на '+'"
    NoOperatorComparisonSelected -> "Нажми на '='"
    NoBoundSet op bound -> format @"У '{op}' напиши '{bound}'" { op: getSymbol op, bound: getSymbol bound }
    BoundMinGreaterThanBoundMax op -> format @"У '{op}' 'Мин' больше, чем 'Макс'" { op: getSymbol op }
    OnlyOneOperandSelected op ->
      let
        op1 /\ op2 = (getOtherOperands op)
      in
        format @"Нажми на '{op1}' или '{op2}'" { op1: getSymbol op1, op2: getSymbol op2 }
    NoExerciseGenerated -> "Придумываю пример..."

flipOperators :: SettingsState -> Operand -> Set FlippedOperator
flipOperators state operand = Set.map (flipOperator operand) state.operatorsSelected

flipOperator :: Operand -> (Operator -> FlippedOperator)
flipOperator = case _ of
  OpA -> \operator -> invertOperator operator false
  OpB -> \operator -> { operator, isFlipped: (operator `elem` [ OpMinus, OpDivide ]) }
  OpC -> \operator -> { operator, isFlipped: false }

type FlippedOperator =
  { operator :: Operator
  , isFlipped :: Boolean
  }

calculateBounds :: Array FlippedOperator -> OtherOperandsBounds -> (Tuple Int Int)
calculateBounds operators { op1Min, op1Max, op2Min, op2Max } = (mini /\ maxi)
  where
  res = concat do
    { operator, isFlipped } <- operators
    (op1 /\ op2) <- do
      op1 <- [ op1Min, op1Max ]
      op2 <- [ op2Min, op2Max ]
      pure (op1 /\ op2)
    let
      f op = [ ((if isFlipped then flip else identity) op) op1 op2 ]
      val =
        case operator of
          OpPlus -> f (+)
          OpMinus -> f (-)
          OpMultiply -> f (*)
          -- a bit hard to estimate so use this approximation
          OpDivide -> [ min op1 op2, max op1 op2, op1 * op2 ]
    pure val
  mini = fromMaybe 0 (minimum res)
  maxi = fromMaybe 0 (maximum res)

invertOperator :: Operator -> Boolean -> FlippedOperator
invertOperator op isFlipped = { operator, isFlipped }
  where
  operator =
    case op of
      OpPlus -> OpMinus
      OpMinus -> OpPlus
      OpMultiply -> OpDivide
      OpDivide -> OpMultiply

mkSettings :: forall m. SettingsState -> H.ComponentHTML Action () m
mkSettings state =
  HH.div
    [ classes $
        [ offcanvas
        , offcanvasBottom
        , h75
        , cSettingsPanel
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

mkOperators :: forall m. SettingsState -> H.ComponentHTML Action () m
mkOperators state =
  HH.div [ classes [ dFlex, justifyContentCenter, pb2 ] ]
    [ HH.div [ classes [ col ] ]
        [ HH.div [ classes [ dFlex, justifyContentCenter ] ]
            ( zip (zip operatorsAll [ justifyContentEnd, justifyContentCenter, justifyContentStart ]) [ 1, 2, 3 ] <#>
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
                  isSelected = Set.member operator state.operatorsComparisonSelected
                in
                  HH.button
                    ( [ classes (settingsButtonClasses isSelected (Set.member NoOperatorComparisonSelected state.buttonErrors))
                      , onClick \_ -> ToggleOperatorComparison operator
                      ]
                        -- TODO don't disable when other comparison operations are available
                        <> singletonIf isSelected aDisabled
                    )
                    [ HH.text $ getSymbol operator
                    ]
              ]
          ]
      ]

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
    isDisabled = state.operandTarget == Just operand
    hasError = hasErrorBound state operand bound
  in
    HH.div [ classes [ col, pb0 ] ]
      [ HH.div [ classes [ formFloating, cBound, ps1, pe1 ] ]
          [ HH.input $
              [ classes $ [ formControl ] <> singletonIf hasError isInvalid
              , id operandInputId
              , placeholder placeholder_
              , value value_
              , onValueInput \inp -> BoundChanged operand bound inp
              , aType "number"
              ] <> singletonIf isDisabled aDisabled
          , HH.label
              [ for operandInputId ]
              [ HH.text $ getSymbol bound ]
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

settingsButtonClasses ∷ Boolean -> Boolean -> Array ClassName
settingsButtonClasses isSelected isError = [ btn, cSelect ] <>
  if isError then [ cError ]
  else singletonIf isSelected cSelected

mkOperandButton ∷ forall m. SettingsState -> Operand → H.ComponentHTML Action () m
mkOperandButton state operand =
  HH.div [ classes [ col2, p1 ] ]
    [ HH.div [ classes [ dFlex, justifyContentCenter ] ]
        [ let
            isDisabled = (state.operandTarget == Just operand)
          in
            HH.button
              ( [ classes $
                    ( settingsButtonClasses
                        ( member operand state.operandsSelected
                            || Just operand == state.operandTarget
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

