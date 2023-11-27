import std/times

import types

proc clockFunction(inter: var Interpreter, args: seq[Value]): Value =
    let time = epochTime()
    return Value(kind: lkNum, numVal: float(time))

let clockBuiltin* = Value(kind: lkFunction, funcVal: LoxFunction(arity: 0,
        call: clockFunction))
