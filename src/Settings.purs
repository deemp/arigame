module Settings where

import Prelude

import Actions (Toggle(..))
import CSSFrameworks.Bootstrap (alignItemsCenter, btn, btnClose, btnGroup, col, col2, dFlex, formControl, formFloating, h75, isInvalid, justifyContentCenter, justifyContentEnd, justifyContentStart, me1, ms1, offcanvas, offcanvasBody, offcanvasBottom, offcanvasHeader, offcanvasTitle, p1, pb0, pb1, pb2, pe1, ps1, pt1, pt2, row, textReset)
import ClassNames (cBound, cError, cSelect, cSelected, cSettings, cSettingsPanel)
import Common (Bound(..), Operand(..), Operator(..), OperatorComparison(..), fixNumber, getOtherOperands, getSymbol, operandsAll, operatorsAll)
import Data.Array (concat, elem, filter, head, zip)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (maximum, minimum)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Lens (Lens', over, (%~), (.~), (?~))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set, member, size)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (length)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (ButtonType(..), classes, for, id, placeholder, tabIndex, type_, value)
import IProps (aAriaLabel, aDataBsDismiss, aDisabled, aType)
import Utils (bw, error, fromJust, singletonIf)

offcanvasBottomId :: String
offcanvasBottomId = "offcanvas-bottom"

data Action
  = ActionToggle Toggle
  | ActionInput Operand Bound String

data SettingsError
  = NoOperandSelected
  | NoOperatorSelected
  | NoOperatorComparisonSelected
  | NoBoundSet Operand Bound
  | BoundMinGreaterThanBoundMax Operand
  | OnlyOneOperandSelected Operand

derive instance Generic SettingsError _
instance Show SettingsError where
  show = genericShow

type State =
  { isOpen :: Boolean
  -- avoid capturing digit input when settings is open
  , operandsSelected :: Set Operand
  , operatorsSelected :: Set Operator
  , operandTarget :: Maybe Operand
  , operandBoundValue :: Map (Operand /\ Bound) String
  , operandBoundValuePlaceholder :: Map (Operand /\ Bound) String
  , operatorsComparisonSelected :: Set OperatorComparison
  , error :: Maybe SettingsError
  }

defaultState :: State
defaultState =
  { isOpen: false
  , operandsSelected: Set.empty
  , operatorsSelected: Set.empty
  , operandTarget: Nothing
  , operandBoundValue: Map.empty
  , operandBoundValuePlaceholder: Map.empty
  , operatorsComparisonSelected: Set.empty
  , error: Just NoOperandSelected
  }

derive instance Eq SettingsError
derive instance Ord SettingsError

type Output = Either SettingsError State

data Query (a :: Type)
  = QueryState (State -> a)
  | QueryToggle Toggle a
  | QueryInput Operand Bound String a

type Slot id = H.Slot Query Output id

component :: forall input m. MonadAff m => H.Component Query input Output m
component = H.mkComponent
  { initialState: const defaultState
  , render: render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  handleQuery :: forall a. Query a -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of
    QueryState reply -> (pure <<< Just <<< reply) =<< H.get
    QueryToggle toggle reply -> handleAction (ActionToggle toggle) $> Just reply
    QueryInput operand bound s reply -> handleAction (ActionInput operand bound s) $> Just reply

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction action =
    do
      case action of
        ActionToggle toggle ->
          do
            case toggle of
              ToggleOperator op -> do
                toggleSelected (bw @"operatorsSelected") op
              ToggleOperatorComparison op -> do
                -- TODO toggle, not only insert
                H.modify_ (over (bw @"operatorsComparisonSelected") %~ Set.insert op)
              ToggleOperand op -> do
                toggleSelected (bw @"operandsSelected") op
              ToggleSettings -> do
                H.modify_ (over (bw @"isOpen") %~ not)
        ActionInput op bound inp -> do
          let inp_ = fixNumber inp
          H.modify_ (over (bw @"operandBoundValue") %~ Map.insert (op /\ bound) inp_)

          -- FIXME Hack to trigger state change and re-render
          when (inp_ /= inp)
            ( do
                handleAction $ ActionInput op bound (inp_ <> "9")
                handleAction $ ActionInput op bound inp_
            )

      H.modify_ update_
      state <- H.get
      H.raise $ maybe (Right state) Left state.error

  toggleSelected :: forall a. Ord a => Lens' State (Set a) -> a -> H.HalogenM State Action () Output m Unit
  toggleSelected l op = H.modify_ (over l %~ Set.toggle op)

-- FIXME
update_ :: State -> State
update_ state = state { error = Nothing } # updateOperators # updateOperandTarget # updateOperandTargetBounds

updateOperators :: State -> State
updateOperators state
  | Set.size state.operatorsSelected == 0 = state # bw @"error" ?~ NoOperatorSelected
  | otherwise = state

updateOperandTarget :: State -> State
updateOperandTarget state = state_
  where
  operands = state.operandsSelected
  newOperand
    | size operands == 2 =
        Right $
          fromJust
            "Set of size 2 doesn't have elements"
            (head (filter (\x -> not (member x operands)) operandsAll))
    | size operands == 1 =
        Left
          $ OnlyOneOperandSelected
          $ fromJust "Set of size 1 doesn't have elements" (Set.findMax operands)
    | Set.isEmpty operands = Left NoOperandSelected
    | otherwise = error "Set can't have 3 operands"
  state_ =
    case newOperand of
      Left err -> state # bw @"error" ?~ err # bw @"operandTarget" .~ Nothing
      Right op -> state # bw @"operandTarget" ?~ op

updateOperandTargetBounds :: State -> State
updateOperandTargetBounds state =
  case state.operandTarget of
    Nothing -> updateOperandTarget state
    Just op ->
      let
        modifyBounds operators =
          let
            modifyVal mod_ = state # bw @"operandBoundValue" %~ mod_
          in
            case getOtherOperandsBounds state op of
              Left err -> modifyVal (Map.delete (op /\ BoundMin) <<< Map.delete (op /\ BoundMax)) # bw @"error" ?~ err
              Right bounds -> do
                let (mini /\ maxi) = calculateBounds (Array.fromFoldable operators) bounds
                modifyVal (Map.insert (op /\ BoundMin) (show mini) <<< Map.insert (op /\ BoundMax) (show maxi))
      in
        modifyBounds (flipOperators state op)

getOtherOperandsBounds :: State -> Operand -> Either SettingsError OtherOperandsBounds
getOtherOperandsBounds state operand = operandsBounds
  where
  (op1 /\ op2) = getOtherOperands operand

  operandBound op bound = maybe (Left (NoBoundSet op bound)) Right (fromString =<< Map.lookup (op /\ bound) state.operandBoundValue)
  operandBounds op = do
    opMin <- operandBound op BoundMin
    opMax <- operandBound op BoundMax
    pure (opMin /\ opMax)

  operandsBounds = do
    (op1Min /\ op1Max) <- operandBounds op1
    when (op1Max < op1Min) (Left (BoundMinGreaterThanBoundMax op1))
    (op2Min /\ op2Max) <- operandBounds op2
    when (op2Max < op2Min) (Left (BoundMinGreaterThanBoundMax op2))
    pure { op1Min, op1Max, op2Min, op2Max }

type OtherOperandsBounds = { op1Min :: Int, op1Max :: Int, op2Min :: Int, op2Max :: Int }

flipOperators :: State -> Operand -> Set FlippedOperator
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

render :: forall m. State -> H.ComponentHTML Action () m
render state =
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
                    , onClick \_ -> ActionToggle ToggleSettings
                    ]
                    []
                ]
            ]
        , HH.div [ classes [ offcanvasBody, pt1 ] ]
            [ HH.div [ classes [ dFlex, justifyContentCenter, pt2, pb2 ] ]
                [ HH.div [ classes colWidths ]
                    ( let
                        mkF :: forall a. (State -> a) -> a
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

mkOperators :: forall m. State -> H.ComponentHTML Action () m
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
                                      (checkHasError state NoOperatorSelected)
                                  )
                              , onClick \_ -> ActionToggle (ToggleOperator x)
                              ]
                              [ HH.text $ getSymbol x
                              ]
                          ]
                      ]
                )
            )
        ]
    ]

mkOperatorComparison :: forall m. State -> H.ComponentHTML Action () m
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
                    ( [ classes
                          ( settingsButtonClasses isSelected
                              (checkHasError state NoOperatorComparisonSelected)
                          )
                      , onClick \_ -> ActionToggle $ ToggleOperatorComparison operator
                      ]
                        -- TODO don't disable when other comparison operations are available
                        <> singletonIf isSelected aDisabled
                    )
                    [ HH.text $ getSymbol operator
                    ]
              ]
          ]
      ]

hasErrorBound :: State -> Operand -> Bound -> Boolean
hasErrorBound state operand bound = (length b == 0) || not (b1 <= b2)
  where
  b = fromMaybe "" (Map.lookup (operand /\ bound) state.operandBoundValue)
  b1 = fromString $ fromMaybe "0" $ Map.lookup (operand /\ BoundMin) state.operandBoundValue
  b2 = fromString $ fromMaybe "0" $ Map.lookup (operand /\ BoundMax) state.operandBoundValue

mkOperandBound ∷ forall m. State -> Operand -> Bound -> H.ComponentHTML Action () m
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
              , onValueInput \inp -> ActionInput operand bound inp
              , aType "number"
              ] <> singletonIf isDisabled aDisabled
          , HH.label
              [ for operandInputId ]
              [ HH.text $ getSymbol bound ]
          ]
      ]

mkOperand ∷ State → Operand → forall m. H.ComponentHTML Action () m
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

mkOperandButton ∷ forall m. State -> Operand -> H.ComponentHTML Action () m
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
                            || isDisabled
                        )
                        (checkHasError state NoOperandSelected)
                    )
                , onClick \_ -> ActionToggle (ToggleOperand operand)
                ]
                  <> singletonIf isDisabled aDisabled
              )
              [ HH.text $ getSymbol operand
              ]
        ]
    ]

checkHasError :: State -> SettingsError -> Boolean
checkHasError state error_ = maybe false ((==) error_) state.error
