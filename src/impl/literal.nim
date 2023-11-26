type
  LiteralKind* = enum lkNum, lkString, lkIden

  Literal* = object
    case kind*: LiteralKind
    of lkString, lkIden: strVal*: string
    of lkNum: numVal*: float


