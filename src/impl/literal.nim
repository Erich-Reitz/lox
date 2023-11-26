type
  LiteralKind* = enum lkBool, lkNum, lkString, lkIden

  Literal* = ref object
    case kind*: LiteralKind
    of lkBool: boolVal*: bool
    of lkString, lkIden: strVal*: string
    of lkNum: numVal*: float

func toString*(l: Literal): string =
  case l.kind
  of lkBool: result = if l.boolVal: "true" else: "false"
  of lkString: result = l.strVal
  of lkIden: result = l.strVal
  of lkNum: result = $l.numVal

