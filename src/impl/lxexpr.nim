import value
import token


type
    ExprKind* = enum ekBinary, ekGrouping, ekValue, ekUnary, ekVar, ekAssign, ekLogical

    BinExpr* = object
        left*: LxExpr
        op*: Token
        right*: LxExpr

    GroupingExpr* = object
        lexpr*: LxExpr

    ValExpr* = object
        val*: Value

    UnaryExpr* = object
        op*: Token
        right*: LxExpr

    VarExpr* = object
        name*: Token

    AssignExpr* = object
        name*: Token
        value*: LxExpr

    LogicalExpr* = object
        left*: LxExpr
        op*: Token
        right*: LxExpr

    LxExpr* = ref object
        case kind*: ExprKind
        of ekBinary: bin*: BinExpr
        of ekGrouping: group*: GroupingExpr
        of ekValue: val*: ValExpr
        of ekUnary: unary*: UnaryExpr
        of ekVar: varex*: VarExpr
        of ekAssign: assign*: AssignExpr
        of ekLogical: logical*: LogicalExpr
