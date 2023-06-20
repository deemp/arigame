module Data where

import Classes

import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude (class Eq, class Ord, class Show)

data Operand = OpA | OpB | OpC

derive instance Eq Operand
derive instance Ord Operand

instance HasSymbol Operand where
  getSymbol = case _ of
    OpA -> "A"
    OpB -> "Б"
    OpC -> "В"

getOtherOperands :: Operand -> Tuple Operand Operand
getOtherOperands =
  case _ of
    OpA -> (OpB /\ OpC)
    OpB -> (OpA /\ OpC)
    OpC -> (OpA /\ OpB)

data Operator = OpPlus | OpMinus | OpMultiply | OpDivide

derive instance Eq Operator
derive instance Ord Operator

instance HasSymbol Operator where
  getSymbol = case _ of
    OpPlus -> "+"
    OpMinus -> "–"
    OpMultiply -> "×"
    OpDivide -> "÷"

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

instance HasSymbol Bound where
  getSymbol = case _ of
    BoundMin -> "Мин"
    BoundMax -> "Макс"

newtype ButtonId = ButtonId Int

derive newtype instance Show ButtonId

operatorsAll :: Array Operator
operatorsAll = [ OpPlus, OpMinus, OpMultiply ]
