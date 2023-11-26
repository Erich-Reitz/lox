import lexceptions
import token
import tokenType

var hadError* = false
var hadRuntimeError* = false

proc report*(line: int, where: string, message: string) =
    echo "[line " & $line & "] Error" & $where & ": " & $message
    hadError = true

proc error*(line: int, message: string) =
    report(line, "", message)

proc error*(token: Token, message: string) =
    if token.typ == tkEOF:
        report(token.line, " at end", message)
    else:
        report(token.line, " at '" & token.lexeme & "'", message)

proc runtimeError*(error: LoxRuntimeError) =
    echo error.msg & "\n[line " & $error.token.line & "]"
    hadRuntimeError = true


