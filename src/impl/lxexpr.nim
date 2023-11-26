import value
import token


type
    ExprKind* = enum ekBinary, ekGrouping, ekValue, ekUnary

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

    LxExpr* = ref object
        case kind*: ExprKind
        of ekBinary: bin*: BinExpr
        of ekGrouping: group*: GroupingExpr
        of ekValue: val*: ValExpr
        of ekUnary: unary*: UnaryExpr
