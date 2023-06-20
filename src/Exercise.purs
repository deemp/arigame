module Exercise where

import Prelude

import Actions (Action)
import CSSFrameworks.Bootstrap (col, dFlex, fwBolder, justifyContentCenter, pe2, ps2, textCenter)
import ClassNames (cErrorMessage, cExercise)
import Classes (class HasSymbol, getSymbol)
import Data (Operand(..), Operator(..), OperatorComparison(..), Unknown(..), getOtherOperands)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lens (Lens', element, traversed, (^.), (^?))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes)
import Settings (SettingsError(..), SettingsState, flipOperator, getOperandTarget, getOtherOperandsBounds, renderError)
import Utils (error, p)

type ExerciseState =
  { operandValue :: Map Operand Int
  , answerCorrect :: Int
  , operatorCurrent :: Operator
  , operatorComparisonCurrent :: OperatorComparison
  , answerCurrent :: String
  , operandTarget :: Operand
  -- empty string means unknown answer
  }

defaultExerciseState :: ExerciseState
defaultExerciseState =
  { operandValue: Map.fromFoldable [ (OpA /\ 40), (OpB /\ 2) ]
  , answerCorrect: 42
  , operatorCurrent: OpPlus
  , operatorComparisonCurrent: OpEqual
  , answerCurrent: ""
  , operandTarget: OpC
  }

-- TODO use local errors
-- data ExerciseError = NoOperatorSelected

mkExercise :: forall m. Either SettingsError ExerciseState -> H.ComponentHTML Action () m
mkExercise state = do
  let
    content =
      case state of
        Left err -> HH.div [ classes [ dFlex, justifyContentCenter, cErrorMessage ] ] [ HH.text $ renderError err ]
        Right state_ ->
          let
            renderOperand :: Operand -> H.ComponentHTML Action () m
            renderOperand op = HH.span [ classes [ ps2, pe2 ] ]
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
              , render_ (p @"operatorCurrent")
              , renderOperand OpB
              , render_ (p @"operatorComparisonCurrent")
              , renderOperand OpC
              ]
  HH.div [ classes [ dFlex, justifyContentCenter ] ]
    [ HH.div [ classes [ col, cExercise ] ]
        [ HH.div [ classes [ fwBolder, textCenter, cExercise ] ]
            [ content
            ]
        ]
    ]

genExercise :: forall m. MonadEffect m => SettingsState -> m (Either SettingsError ExerciseState)
genExercise state = exState
  where
  exState =
    case getOperandTarget state of
      Left err -> pure (Left err)
      Right operandTarget ->
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
                  pure $ Right $
                    { operandValue: Map.fromFoldable [ (op1 /\ val1), (op2 /\ val2) ]
                    , answerCorrect: ans
                    , operatorCurrent: operator_
                    ,
                      -- TODO select randomly
                      operatorComparisonCurrent: OpEqual
                    , answerCurrent: ""
                    , operandTarget
                    }
