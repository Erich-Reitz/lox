import math

type
  ValueKind* = enum lkBool, lkNum, lkString, lkIden

  Value* = ref object
    case kind*: ValueKind
    of lkBool: boolVal*: bool
    of lkString, lkIden: strVal*: string
    of lkNum: numVal*: float

func isInteger(value: float, epsilon: float = 1e-10): bool =
  return abs(ceil(value) - value) < epsilon or abs(floor(value) - value) < epsilon

func loxRepr(value: float): string =
  if isInteger(value):
    result = $int(value)
  else:
    result = $value

proc `$`*(v: Value): string =
  case v.kind
  of lkBool: result = if v.boolVal: "true" else: "false"
  of lkString, lkIden: result = v.strVal
  of lkNum: result = loxRepr(v.numVal)
