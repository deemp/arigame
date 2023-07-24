module Main where

import Prelude

import Actions (Toggle(..))
import CSSFrameworks.Bootstrap (col)
import Common (Bound(..), Operand(..), Operator(..), OperatorComparison(..), getOtherOperands)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Lens (element, traversed, (^?))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Error, Milliseconds(..), delay, forkAff, joinFiber, killFiber, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception as Exception
import Effect.Random (randomInt)
import Exercise (ExerciseState, OperandTargetStatus(..))
import Exercise as Exercise
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes)
import Halogen.VDom.Driver (runUI)
import Header as Header
import Keyboard (Output(..))
import Keyboard as Keyboard
import Settings (SettingsError(..), flipOperator, getOtherOperandsBounds)
import Settings as Settings
import Type.Prelude (Proxy(..))
import Utils (error, fromJust)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

type State = Unit

initialState :: Unit
initialState = unit

component :: forall query input output m. MonadThrow Error m => MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: const initialState
    , render: const root
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just ActionInit
        }
    }

data Action
  = ActionKeyboard Keyboard.Output
  | ActionSettings Settings.Output
  | ActionInit

handleAction :: forall output m. MonadThrow Error m => MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction =
  case _ of
    ActionInit -> do
      traverse_ (H.tell _settings unit <<< Settings.QueryToggle)
        [ ToggleOperand OpA
        , ToggleOperand OpB
        , ToggleOperator OpMinus
        , ToggleOperatorComparison OpEqual
        ]
      traverse_ (H.tell _settings unit) $
        (\(op /\ bound /\ s) -> Settings.QueryInput op bound s) <$>
          [ (OpA /\ BoundMin /\ "10")
          , (OpA /\ BoundMax /\ "20")
          , (OpB /\ BoundMin /\ "0")
          , (OpB /\ BoundMax /\ "10")
          ]
    ActionKeyboard a ->
      case a of
        OutputClicked button -> do
          answerCurrent <- H.request _exercise unit (Exercise.QueryButtonClicked button)
          maybe (pure unit)
            ( \x ->
                do
                  H.tell _header unit (Header.QueryIncrementCorrect x)
                  state <- H.request _settings unit Settings.QueryState
                  maybe (pure unit) genExercise_ state
            )
            answerCurrent
    ActionSettings a ->
      case a of
        Left err -> H.tell _exercise unit (Exercise.QuerySetState (Exercise.StateError $ Exercise.SettingsError err))
        Right s -> genExercise_ s

  where

  genExercise_ :: Settings.State -> H.HalogenM State Action Slots output m Unit
  genExercise_ state = do
    H.tell _exercise unit (Exercise.QuerySetState Exercise.StateWaiting)
    exerciseF <- liftAff $ forkAff $ genExercise state
    void $ liftAff $ forkAff $ delay (Milliseconds 1000.0) *> killFiber (Exception.error "timeout") exerciseF
    exercise <- liftAff $ try $ joinFiber exerciseF
    liftEffect $ Console.log (show exercise)
    let
      exercise_ =
        case exercise of
          Left _ -> Exercise.StateError Exercise.NoExerciseGenerated
          Right ex -> either (Exercise.StateError <<< Exercise.SettingsError) Exercise.StateReady ex
    H.tell _exercise unit (Exercise.QuerySetState exercise_)

genExercise :: forall m. MonadAff m => Settings.State -> m (Either SettingsError ExerciseState)
genExercise state = exState
  where
  exState =
    case state.operandTarget of
      Nothing -> pure $ Left $ fromJust "Target operand undefined, but no error was recorded." state.error
      Just operandTarget ->
        let
          (op1 /\ op2) = getOtherOperands operandTarget
          bounds = getOtherOperandsBounds state operandTarget
          operators = Array.fromFoldable state.operatorsSelected
        in
          case bounds of
            Left err -> pure $ Left err
            Right bounds_ -> do
              operatorIdx <- liftEffect $ randomInt 0 (max (Array.length operators - 1) 0)
              let operator = operators ^? element operatorIdx traversed
              case operator of
                Nothing -> pure $ Left NoOperatorSelected
                Just operator_ -> do
                  (val1 /\ val2 /\ ans) <-
                    case flipOperator operandTarget operator_ of
                      { isFlipped, operator: OpDivide } -> do
                        -- TODO
                        pure (1 /\ 2 /\ 3)
                      x -> do
                        val1 <- liftEffect $ randomInt bounds_.op1Min bounds_.op1Max
                        val2 <- liftEffect $ randomInt bounds_.op2Min bounds_.op2Max
                        let
                          f op = (if x.isFlipped then flip else identity) op val1 val2
                          ans =
                            case x.operator of
                              OpPlus -> f (+)
                              OpMinus -> f (-)
                              OpMultiply -> f (*)
                              OpDivide -> error "genExercise: Division was pattern matched above"
                        pure (val1 /\ val2 /\ ans)
                  pure $ Right
                    { operandValue: Map.fromFoldable [ (op1 /\ val1), (op2 /\ val2) ]
                    , answerCorrect: ans
                    , operatorCurrent: operator_
                    ,
                      -- TODO select randomly
                      operatorComparisonCurrent: OpEqual
                    , answerCurrent: ""
                    , operandTarget
                    , operandTargetStatus: StatusIncomplete
                    }

type Slots =
  ( exercise :: Exercise.Slot Unit
  , keyboard :: Keyboard.Slot Unit
  , header :: Header.Slot Unit
  , settings :: Settings.Slot Unit
  )

_exercise = Proxy :: Proxy "exercise"

_keyboard = Proxy :: Proxy "keyboard"

_header = Proxy :: Proxy "header"

_settings = Proxy :: Proxy "settings"

root :: forall m. MonadAff m => H.ComponentHTML Action Slots m
root =
  HH.div [ classes [ col ] ]
    [ HH.slot_ _header unit Header.component unit
    , HH.slot_ _exercise unit Exercise.component unit
    , HH.slot _keyboard unit Keyboard.component unit ActionKeyboard
    , HH.slot _settings unit Settings.component unit ActionSettings
    ]