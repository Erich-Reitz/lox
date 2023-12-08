import std/os


import impl/lexer
import impl/interpreter
import impl/parser
import impl/status

proc run(program: string): void =
    let tokens = lex(program)
    let lexpr = parse(tokens)
    if status.hadError:
        return
    interpret(lexpr)


proc runfile(filename: string): int =
    try:
        let contents = readFile(filename)
        run(contents)
        if hadError:
            return 65
        if hadRuntimeError:
            return 70

        return 0


    except IOError as e:
        echo e.msg
        return QuitFailure

proc main() =
    let paramCount = os.paramCount()
    if paramCount != 1:
        echo "Usage: ./nlox [script]"
        quit(QuitFailure)
    else:
        let filename = os.paramStr(1)
        let result = runfile(filename)
        quit(result)


when isMainModule:
    main()
