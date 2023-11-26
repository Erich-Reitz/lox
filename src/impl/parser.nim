import literal
import lxexpr
import status
import token
import tokenType


type Parser* = object
    tokens*: seq[Token]
    current*: int = 0

type ParseError = object of CatchableError

func peek(p: Parser): Token =
    p.tokens[p.current]

func isAtEnd(p: Parser): bool =
    peek(p).typ == tkEOF

func previous(p: Parser): Token =
    p.tokens[p.current - 1]

func check(p: Parser, typ: TokenType): bool =
    if isAtEnd(p):
        return false

    peek(p).typ == typ

func advance(p: var Parser): Token =
    if isAtEnd(p) == false:
        p.current += 1

    previous(p)

func synchronize(p: var Parser) =
    discard advance(p)

    while isAtEnd(p) == false:
        if previous(p).typ == tkSemicolon:
            return

        case peek(p).typ:
            of tkClass, tkFun, tkVar, tkFor, tkIf, tkWhile, tkPrint, tkReturn:
                return
            else:
                discard

        discard advance(p)


proc consume(p: var Parser, typ: TokenType, msg: string): Token =
    if check(p, typ) == true:
        return advance(p)

    error(peek(p), msg)
    raise newException(ParseError, msg)



func match(p: var Parser, typ: TokenType): bool =
    if check(p, typ):
        discard advance(p)
        return true

    return false

proc match*(p: var Parser, types: varargs[TokenType]): bool =
    for typ in types:
        if match(p, typ):
            return true

    return false


proc expression(p: var Parser): LxExpr

proc primary(p: var Parser): LxExpr =
    if match(p, tkFalse):
        return LxExpr(kind: ekLiteral, lit: LiteralExpr(value: Literal(
                kind: lkBool, boolVal: false)))

    elif match(p, tkTrue):
        return LxExpr(kind: ekLiteral, lit: LiteralExpr(value: Literal(
                kind: lkBool, boolVal: true)))

    elif match(p, tkNil):
        return LxExpr(kind: ekLiteral, lit: LiteralExpr(value: nil))
    elif match(p, tkNumber, tkString):
        return LxExpr(kind: ekLiteral, lit: LiteralExpr(value: previous(p).literal))
    elif match(p, tkLeftParen):
        let lexpr = expression(p)
        discard consume(p, tkRightParen, "expect ')' after expression.")
        return LxExpr(kind: ekGrouping, group: GroupingExpr(lexpr: lexpr))
    else:
        error(peek(p), "expect expression.")
        raise newException(ParseError, "expect expression.")

proc unary(p: var Parser): LxExpr =
    if match(p, tkBang, tkMinus):
        let op = previous(p)
        let right = unary(p)
        return LxExpr(kind: ekUnary, unary: UnaryExpr(op: op, right: right))

    primary(p)


proc factor(p: var Parser): LxExpr =
    var lexpr = unary(p)

    while match(p, tkSlash, tkStar):
        let op = previous(p)
        let right = unary(p)
        lexpr = LxExpr(kind: ekBinary, bin: BinExpr(left: lexpr, op: op, right: right))

    lexpr

proc term(p: var Parser): LxExpr =
    var lexpr = factor(p)

    while match(p, tkMinus, tkPlus):
        let op = previous(p)
        let right = factor(p)
        lexpr = LxExpr(kind: ekBinary, bin: BinExpr(left: lexpr, op: op, right: right))

    lexpr

proc comparison(p: var Parser): LxExpr =
    var lexpr = term(p)

    while match(p, tkGreater, tkGreaterEqual, tkLess, tkLessEqual):
        let op = previous(p)
        let right = term(p)
        lexpr = LxExpr(kind: ekBinary, bin: BinExpr(left: lexpr, op: op, right: right))

    lexpr

proc equality(p: var Parser): LxExpr =
    var lexpr = comparison(p)

    while match(p, tkBangEqual, tkEqualEqual):
        let op = previous(p)
        let right = comparison(p)
        lexpr = LxExpr(kind: ekBinary, bin: BinExpr(left: lexpr, op: op, right: right))

    lexpr

proc expression(p: var Parser): LxExpr =
    equality(p)

proc parse*(tokens: seq[Token]): LxExpr =
    var p = Parser(tokens: tokens)
    try:
        return expression(p)
    except ParseError:
        return nil
