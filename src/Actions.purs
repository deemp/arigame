module Actions where

import Common (Bound, ButtonId, Operand, Operator, OperatorComparison)


data Button
  = ButtonNumber ButtonId
  | ButtonMinus
  | ButtonDelete

data Toggle
  = ToggleOperator Operator
  | ToggleOperatorComparison OperatorComparison
  | ToggleOperand Operand
  | ToggleSettings
