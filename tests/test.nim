import std/osproc
import std/unittest

func testFileLocation(testname: string): string =
  "tests/" & testname & "/" & testname & ".lox"

func testOutputLocation(testname: string): string =
  "tests/" & testname & "/" & testname & ".out"

proc expectedTestOutput(testname: string): string =
  readFile(testOutputLocation(testname))

proc execTestCaptureOutput(testname: string): (string, int) =
  osproc.execCmdEx("./nlox " & testFileLocation(testname))

proc runTestFile(testname: string, expectsError: bool = false): string =
  let (output, exitcode) = execTestCaptureOutput(testname)
  if not expectsError:
    assert exitcode == 0
  return output

proc runTest(testname: string, expectsError: bool = false): bool =
  let output = runTestFile(testname, expectsError)
  let expectedOutput = expectedTestOutput(testname)
  result = output == expectedOutput

suite "integration tests":
  # compile main program once before executing tests
  let res = osproc.execCmd("nimble build")
  if res != 0:
    echo "failed to compile"
    quit(QuitFailure)
  
  setup:
    discard
    
  test "assignment_associativity":
    check runTest("assignment_associativity")
  
  test "assignment_global":
    check runTest("assignment_global")
  
  test "block_scope":
    check runTest("block_scope")
  
  test "block_empty":
    check runTest("block_empty") 

  test "bool_not":
    check runTest("bool_not")

  test "bool_equality":
    check runTest("bool_equality")

  test "class_empty":
    check runTest("class_empty")
  
  test "class_inherit_self":
    check runTest("class_inherit_self", expectsError = true)

  test "class_inherited_method":
    check runTest("class_inherited_method") 

  test "class_local_inherit_other":
    check runTest("class_local_inherit_other")

  test "class_local_inherit_self":
    check runTest("class_local_inherit_self", expectsError = true)

  test "class_reference_self":
    check runTest("class_reference_self")

  test "closure_nested_closure":
    check runTest("closure_nested_closure")
  
  test "closure_reference_closure_multiple_times":
    check runTest("closure_reference_closure_multiple_times")
  
  test "closure_close_over_function_parameter":
    check runTest("closure_close_over_function_parameter")  

  test "constructor_arguments":
    check runTest("constructor_arguments")

  test "constructor_call_init_early_return":
    check runTest("constructor_call_init_early_return")

  test "constructor_return_value":
    check runTest("constructor_return_value", expectsError = true)

  test "logical_op_and_truth":
    check runTest("logical_op_and_truth")