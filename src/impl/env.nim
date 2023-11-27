import std/tables

import types


func initEnv*(enclosing: Env): Env =
    var env = Env()
    env.enclosing = enclosing
    env.values = initTable[string, Value]()
    return env

func define*(env: Env, name: string, value: Value) =
    env.values[name] = value

func get*(env: Env, name: Token): Value =
    if env.values.hasKey(name.lexeme):
        return env.values[name.lexeme]

    if env.enclosing != nil:
        return env.enclosing.get(name)

    let msg = "Undefined variable '" & name.lexeme & "'."
    var exception = newException(LoxUndefinedVariable, msg)
    exception.token = name
    raise exception

func assign*(env: Env, name: Token, value: Value) =
    if env.values.hasKey(name.lexeme):
        env.values[name.lexeme] = value
        return

    if env.enclosing != nil:
        env.enclosing.assign(name, value)
        return

    let msg = "Undefined variable '" & name.lexeme & "'."
    var exception = newException(LoxUndefinedVariable, msg)
    exception.token = name
    raise exception



