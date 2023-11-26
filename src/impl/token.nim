import std/options

import literal
import tokenType

type Token* = object
    typ*: TokenType
    literal*: Option[Literal]
    lexeme*: string
    line*: int

func toString*(t: Token): string =
    return $t.typ & " " & t.lexeme & " " & $t.line
