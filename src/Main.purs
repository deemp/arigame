module Main where

import Prelude

import Actions (Action(..))
import CSSFrameworks.Bootstrap (col)
import Data (Bound(..), ButtonId(..), Operand(..), Operator(..), OperatorComparison(..))
import Data.Array (filter, head)
import Data.Either (Either(..), either, hush)
import Data.Lens (Lens', _Right, over, view, (%~), (.~))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, member, size)
import Data.Set as Set
import Data.String (length, take)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Exercise (ExerciseState, genExercise, mkExercise)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes)
import Halogen.VDom.Driver (runUI)
import Header (HeaderState, mkHeader)
import Keyboard (mkKeyboard)
import Settings (SettingsError(..), SettingsState, mkSettings, updateTargetOperandBounds)
import Utils (error, p)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: const initialState
    , render: root
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

type State =
  { headerState :: HeaderState
  , settingsState :: SettingsState
  , exerciseState :: Either SettingsError ExerciseState
  }

initialState :: State
initialState =
  { headerState:
      { counterAnswersCorrect: 0
      , counterAnswersIncorrect: 0
      }
  , exerciseState: Left NoOperandSelected
  , settingsState:
      { isOpen: false
      , operandsSelected: Set.empty
      , operatorsSelected: Set.empty
      , operandBoundValue: Map.empty
      , operandBoundValuePlaceholder: Map.empty
      , operatorsComparisonSelected: Set.empty
      , operandTarget: Nothing
      , buttonErrors: Set.empty
      }
  }

type St = { someField :: Int }

st :: St
st = { someField: 1 }

operandsAll ∷ Array Operand
operandsAll = [ OpA, OpB, OpC ]

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

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction =
  case _ of
    NumberButtonClicked (ButtonId bid) -> do
      modifyAnswerCurrent (\x -> fixNumber (x <> show bid))
      checkAnswer_
    MinusButtonClicked -> do
      modifyAnswerCurrent (\x -> fixNumber ("-" <> x))
    DeleteButtonClicked ->
      modifyAnswerCurrent (\x -> take (max (length x - 1) 0) x)
    ToggleOperator op -> do
      toggleSelected (p @"operatorsSelected") op
      toggleNoSelectedError (p @"operatorsSelected") NoOperatorSelected
      updateTargetOperandBounds_
      genExercise_
    ToggleOperatorComparison op -> do
      H.modify_ (over (p @"settingsState" <<< p @"operatorsComparisonSelected") %~ Set.insert op)
      toggleNoSelectedError (p @"operatorsComparisonSelected") NoOperatorComparisonSelected
      updateTargetOperandBounds_
      genExercise_
    ToggleOperand op -> do
      toggleSelected (p @"operandsSelected") op

      -- if 2 operands selected, set target operand
      operands <- H.gets _.settingsState.operandsSelected
      let
        operand
          | size operands == 2 = maybe (error "set has 3 operands") Right (head (filter (\x -> not (member x operands)) operandsAll))
          | size operands == 1 = Left (OnlyOneOperandSelected (maybe (error "set has one element, but no max") identity (Set.findMax operands)))
          | Set.isEmpty operands = Left NoOperandSelected
          | otherwise = error "set has 3 operands"
      H.modify_ (over (p @"settingsState" <<< p @"operandTarget") .~ hush operand)

      toggleNoSelectedError (p @"operandsSelected") NoOperandSelected

      case operand of
        Left err -> H.modify_ (_ { exerciseState = Left err })
        Right _ -> do
          updateTargetOperandBounds_
          genExercise_
    ToggleSettings -> do
      H.modify_ (over (p @"settingsState" <<< p @"isOpen") %~ not)
    BoundChanged op bound inp -> do
      let inp_ = fixNumber inp
      H.modify_ (over (p @"settingsState" <<< p @"operandBoundValue") %~ Map.insert (op /\ bound) (fixNumber inp))

      -- hack to re-render
      when (fixNumber inp /= inp)
        ( do
            handleAction $ BoundChanged op bound (inp_ <> "9")
            handleAction $ BoundChanged op bound inp_
        )

      updateTargetOperandBounds_
      genExercise_
    Init -> do
      handleAction $ ToggleOperand OpA
      handleAction $ ToggleOperand OpB
      handleAction $ ToggleOperator OpMinus
      handleAction $ ToggleOperatorComparison OpEqual

      handleAction $ BoundChanged OpA BoundMin "10"
      handleAction $ BoundChanged OpA BoundMax "20"
      handleAction $ BoundChanged OpB BoundMin "0"
      handleAction $ BoundChanged OpB BoundMax "10"
      updateTargetOperandBounds_
      genExercise_
  where
  toggleNoSelectedError :: forall a. MonadAff m => Lens' SettingsState (Set a) -> SettingsError -> H.HalogenM State Action () output m Unit
  toggleNoSelectedError l err = do
    os <- H.gets (view (p @"settingsState" <<< l))
    H.modify_
      ( over (p @"settingsState" <<< p @"buttonErrors") %~
          if
            Set.isEmpty os then Set.insert err
          else Set.delete err
      )

  toggleSelected :: forall a. Ord a => MonadAff m => Lens' SettingsState (Set a) -> a -> H.HalogenM State Action () output m Unit
  toggleSelected l op = H.modify_ (over (p @"settingsState" <<< l) %~ Set.toggle op)

  modifyAnswerCurrent :: (String -> String) -> H.HalogenM State Action () output m Unit
  modifyAnswerCurrent f = H.modify_ $ over (p @"exerciseState" <<< _Right <<< p @"answerCurrent") %~ f

  updateTargetOperandBounds_ :: H.HalogenM State Action () output m Unit
  updateTargetOperandBounds_ = H.modify_ (over (p @"settingsState") %~ updateTargetOperandBounds)

  genExercise_ :: H.HalogenM State Action () output m Unit
  genExercise_ = H.gets _.settingsState >>= genExercise >>= \x -> H.modify_ (_ { exerciseState = x })

  checkAnswer_ :: H.HalogenM State Action () output m Unit
  checkAnswer_ = do
    state <- H.get
    case state.exerciseState of
      Left _ -> pure unit
      Right exerciseState -> do
        let
          f :: (State -> State) -> H.HalogenM State Action () output m Unit
          f upd = (H.liftAff $ delay $ Milliseconds 1500.0) *> H.modify_ upd *> genExercise_
        if exerciseState.answerCurrent == show exerciseState.answerCorrect then
          f
            ( \x ->
                x # p @"headerState" <<< p @"counterAnswersCorrect" %~ (_ + 1)
                  # p @"exerciseState" .~ Left NoExerciseGenerated
            )
        else if length exerciseState.answerCurrent == length (show exerciseState.answerCorrect) then
          f (_ # p @"headerState" <<< p @"counterAnswersIncorrect" %~ (_ + 1))
        else pure unit


root :: forall m. State -> H.ComponentHTML Action () m
root state =
  HH.div [ classes [ col ] ]
    [ mkHeader state.headerState
    , mkExercise state.exerciseState
    , mkKeyboard
    , mkSettings state.settingsState
    ]