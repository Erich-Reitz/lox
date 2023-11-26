type
    TokenType* = enum
        tkLeftParen, tkRightParen, tkLeftBrace, tkRightBrace,
        tkComma, tkDot, tkMinus, tkPlus, tkSemicolon, tkSlash, tkStar,

        # One or two character tokens.
        tkBang, tkBangEqual,
        tkEqual, tkEqualEqual,
        tkGreater, tkGreaterEqual,
        tkLess, tkLessEqual,

        # Literals.
        tkIdentifier, tkString, tkNumber,

        # Keywords.
        tkAnd, tkClass, tkElse, tkFalse, tkFun, tkFor, tkIf, tkNil, tkOr,
        tkPrint, tkReturn, tkSuper, tkThis, tkTrue, tkVar, tkWhile,

        tkEOF

