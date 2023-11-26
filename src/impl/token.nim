import value
import tokenType

type Token* = object
  typ*: TokenType
  value*: Value
  lexeme*: string
  line*: int
