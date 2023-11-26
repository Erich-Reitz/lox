import token

type
    LoxRuntimeError* = ref object of CatchableError
        token*: Token

    LoxInvalidCast* = object of LoxRuntimeError

type
    LoxParseError* = object of CatchableError
