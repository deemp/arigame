module Common where


import Prelude

import Data.Either (either)
import Data.String.Regex (replace, regex)
import Data.String.Regex.Flags (global)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

class HasSymbol a where
  getSymbol :: a -> String

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

operandsAll ∷ Array Operand
operandsAll = [ OpA, OpB, OpC ]

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