module Actions where

import Data (Bound, ButtonId, Operand, Operator, OperatorComparison)

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
