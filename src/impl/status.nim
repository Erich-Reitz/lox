var hadError* = false

proc report*(line: int, where: string, message: string) =
  echo "[line " & $line & "] Error" & $where & ": " & $message
  hadError = true

proc error*(line: int, message: string) =
  report(line, "", message)
