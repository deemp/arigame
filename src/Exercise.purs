module Exercise where

import Prelude

import Actions (Button(..))
import CSSFrameworks.Bootstrap (col, dFlex, fwBolder, justifyContentCenter, pe2, ps2, textCenter)
import ClassNames (cCorrect, cErrorMessage, cExercise, cIncorrect)
import Common (class HasSymbol, Operand(..), Operator(..), OperatorComparison(..), Unknown(..), fixNumber, getOtherOperands, getSymbol)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', over, (%~), (.~), (^.))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String (take, length)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes)
import Record.Format (format)
import Settings (SettingsError(..))
import Utils (bw)

type ExerciseState =
  { operandValue :: Map Operand Int
  , answerCorrect :: Int
  , operatorCurrent :: Operator
  , operatorComparisonCurrent :: OperatorComparison
  , answerCurrent :: String
  , operandTarget :: Operand
  -- empty string means unknown answer
  , operandTargetStatus :: OperandTargetStatus
  }

data OperandTargetStatus
  = StatusIncomplete
  | StatusIsCorrect Boolean

data ExerciseError
  = SettingsError SettingsError
  | NoExerciseGenerated

data State
  = StateError ExerciseError
  | StateWaiting
  | StateReady ExerciseState

derive instance Generic State _

defaultState :: State
defaultState = StateWaiting

data Query (a :: Type)
  = QueryButtonClicked Button (Boolean -> a)
  | QuerySetState State a

data Action = ActionInit

type Slot id = forall output. H.Slot Query output id

component :: forall input output m. MonadAff m => H.Component Query input output m
component = H.mkComponent
  { initialState: const defaultState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just ActionInit
      , handleQuery = handleQuery
      }
  }
  where
  -- TODO handle input
  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    ActionInit -> H.modify_ (const StateWaiting)

  handleQuery :: forall a. Query a -> H.HalogenM State Action () output m (Maybe a)
  handleQuery = case _ of
    QueryButtonClicked button reply ->
      case button of
        ButtonNumber bid -> do
          modifyAnswerCurrent (\x -> fixNumber (x <> show bid))
          checkAnswer_
        ButtonMinus -> do
          modifyAnswerCurrent (\x -> fixNumber ("-" <> x))
          pure Nothing
        ButtonDelete -> do
          modifyAnswerCurrent (\x -> take (max (length x - 1) 0) x)
          pure Nothing
      where
      modifyAnswerCurrent :: (String -> String) -> H.HalogenM State Action () output m Unit
      modifyAnswerCurrent f = H.modify_ $ over (bw @"%StateReady.answerCurrent") %~ f

      checkAnswer_ :: H.HalogenM State Action () output m (Maybe a)
      checkAnswer_ = do
        state <- H.get
        let
          result =
            case state of
              StateReady state_
                | state_.answerCurrent == show state_.answerCorrect -> Just true
                | length state_.answerCurrent == length (show state_.answerCorrect) -> Just false
              _ -> Nothing
        -- TODO show correct answer
        case result of
          Nothing -> H.modify_ (over (bw @"%StateReady.operandTargetStatus") .~ StatusIncomplete)
          Just c -> do
            H.modify_ (over (bw @"%StateReady.operandTargetStatus") .~ StatusIsCorrect c)
            liftAff $ delay (Milliseconds 500.0)
            unless c do
              H.modify_ (over (bw @"%StateReady") %~ \s -> s { answerCurrent = show s.answerCorrect })
              H.modify_ (over (bw @"%StateReady.operandTargetStatus") .~ StatusIsCorrect true)
              liftAff $ delay (Milliseconds 700.0)
        pure $ reply <$> result
    QuerySetState state a -> do
      H.modify_ $ const state
      pure $ Just a

renderError :: ExerciseError -> String
renderError =
  case _ of
    SettingsError err ->
      case err of
        NoOperandSelected -> format @"Нажми на '{a}' и '{b}'" { a: getSymbol OpA, b: getSymbol OpB }
        NoOperatorSelected ->
          format
            @"Нажми на '{plus}', '{minus}' или '{multiply}'"
            { plus: getSymbol OpPlus, minus: getSymbol OpMinus, multiply: getSymbol OpMultiply }
        NoOperatorComparisonSelected -> format @"Нажми на '{eq}'" { eq: getSymbol OpEqual }
        NoBoundSet op bound -> format @"У '{op}' напиши '{bound}'" { op: getSymbol op, bound: getSymbol bound }
        BoundMinGreaterThanBoundMax op -> format @"У '{op}' 'Мин' больше, чем 'Макс'" { op: getSymbol op }
        OnlyOneOperandSelected op ->
          let
            op1 /\ op2 = (getOtherOperands op)
          in
            format @"Нажми на '{op1}' или '{op2}'" { op1: getSymbol op1, op2: getSymbol op2 }
    NoExerciseGenerated -> "Не смог придумать пример :("

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  let
    content =
      case state of
        StateError err ->
          HH.div
            [ classes [ dFlex, justifyContentCenter, cErrorMessage ] ]
            [ HH.text $ renderError err ]
        StateReady state_ ->
          let
            csAnswerStatus =
              case state_.operandTargetStatus of
                StatusIncomplete -> []
                StatusIsCorrect c -> [ if c then cIncorrect else cCorrect ]

            renderOperand :: Operand -> H.ComponentHTML Action () m
            renderOperand op = HH.span [ classes ([ ps2, pe2 ] <> csAnswerStatus) ]
              [ HH.text
                  ( if state_.operandTarget == op then
                      if state_.answerCurrent == "" then getSymbol Unknown else state_.answerCurrent
                    else maybe
                      (getSymbol Unknown)
                      (\x -> (if x < 0 then (\y -> "(" <> y <> ")") else identity) (show x))
                      (Map.lookup op state_.operandValue)
                  )
              ]

            render_ :: forall a. HasSymbol a => Lens' ExerciseState a -> H.ComponentHTML Action () m
            render_ l = HH.span [ classes [ ps2, pe2 ] ] [ HH.text (getSymbol (state_ ^. l)) ]
          in
            HH.p_
              [ renderOperand OpA
              , render_ (bw @"operatorCurrent")
              , renderOperand OpB
              , render_ (bw @"operatorComparisonCurrent")
              , renderOperand OpC
              ]
        StateWaiting ->
          HH.p_ [ HH.text "Придумываю пример..." ]
  HH.div [ classes [ dFlex, justifyContentCenter ] ]
    [ HH.div [ classes [ col, cExercise ] ]
        [ HH.div [ classes [ fwBolder, textCenter, cExercise ] ]
            [ content
            ]
        ]
    ]
