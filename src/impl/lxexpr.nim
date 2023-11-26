import literal
import token


type
    ExprKind* = enum ekBinary, ekGrouping, ekLiteral, ekUnary

    BinExpr* = object
        left*: LxExpr
        op*: Token
        right*: LxExpr

    GroupingExpr* = object
        lexpr*: LxExpr

    LiteralExpr* = object
        value*: Literal

    UnaryExpr* = object
        op*: Token
        right*: LxExpr

    LxExpr* = ref object
        case kind*: ExprKind
        of ekBinary: bin*: BinExpr
        of ekGrouping: group*: GroupingExpr
        of ekLiteral: lit*: LiteralExpr
        of ekUnary: unary*: UnaryExpr


func toString*(self: LxExpr): string

func toString*(self: BinExpr): string =
    "(" & self.op.lexeme & " " & toString(self.left) & " " & toString(
            self.right) & ")"

func toString*(self: GroupingExpr): string =
    "(" & toString(self.lexpr) & ")"

func toString*(self: LiteralExpr): string =
    toString(self.value)

func toString*(self: UnaryExpr): string =
    "(" & self.op.lexeme & " " & toString(self.right) & ")"

func toString*(self: LxExpr): string =
    case self.kind
    of ekBinary: result = toString(self.bin)
    of ekGrouping: result = toString(self.group)
    of ekLiteral: result = toString(self.lit)
    of ekUnary: result = toString(self.unary)
