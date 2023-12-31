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

  test "logical_op_or":
    check runTest("logical_op_or")
  
  test "logical_op_and":
    check runTest("logical_op_and")
  
  test "while_closure_in_body":
    check runTest("while_closure_in_body")

  test "while_return_closure":
    check runTest("while_return_closure")

  test "while_syntax":
    check runTest("while_syntax")

  test "super_constructor":
    check runTest("super_constructor")

  test "print_missing_argument":
    check runTest("print_missing_argument", expectsError = true)  
  
  test "function_mutual_recursion":
    check runTest("function_mutual_recursion")
  
  # yes, it really does expect an error
  test "local_mutual_recursion":
    check runTest("local_mutual_recursion", expectsError = true)

  test "function_nested_call_with_arguments":
    check runTest("function_nested_call_with_arguments")
  
  test "for_syntax":
    check runTest("for_syntax")

  test "field_call_function_field":
    check runTest("field_call_function_field")

  test "field_call_nonfunction_field":
    check runTest("field_call_nonfunction_field", expectsError = true)

  test "field_get_and_set_method":
    check runTest("field_get_and_set_method")

  test "variable_shadow_and_local":
    check runTest("variable_shadow_and_local")

  test "variable_shadow_global":
    check runTest("variable_shadow_global")

  test "variable_collide_with_parameter":
    check runTest("variable_collide_with_parameter", expectsError = true)

  test "variable_duplicate_local":
    check runTest("variable_duplicate_local", expectsError = true)

  test "variable_duplicate_parameter":
    check runTest("variable_duplicate_parameter", expectsError = true)

  test "variable_early_bound":
    check runTest("variable_early_bound")

  test "this_this_in_method":
    check runTest("this_this_in_method")
  
  test "this_closure":
    check runTest("this_closure")
  
  test "this_nested_class":
    check runTest("this_nested_class")

  test "this_nested_closure":
    check runTest("this_nested_closure")

  test "this_this_at_top_level":
    check runTest("this_this_at_top_level", expectsError = true)

  test "this_this_in_top_level_function":
    check runTest("this_this_in_top_level_function", expectsError = true)

  test "inheritance_constructor":
    check runTest("inheritance_constructor")

  test "inheritance_inherit_methods":
    check runTest("inheritance_inherit_methods")

  test "inheritance_inherit_fields":
    check runTest("inheritance_inherit_fields")

  test "if_if_truth":
    check runTest("if_if_truth")

  test "if_if":
    check runTest("if_if")

  test "return_return_after_if":
    check runTest("return_return_after_if")

  test "return_return_after_else":
    check runTest("return_return_after_else")

  test "return_return_after_while":
    check runTest("return_return_after_while")

  test "return_in_method":
    check runTest("return_in_method")

  test "variable_scope_reuse_in_different_blocks":
    check runTest("variable_scope_reuse_in_different_blocks")

  test "operator_add":
    check runTest("operator_add")

  test "operator_comparison":
    check runTest("operator_comparison") 
  
  test "operator_divide":
    check runTest("operator_divide")
  
  test "operator_equals":
    check runTest("operator_equals")
  
  test "operator_equals_class":
    check runTest("operator_equals_class")
  
  test "operator_equals_method":
    check runTest("operator_equals_method")
  
  test "operator_multiply":
    check runTest("operator_multiply")

  test "operator_negate":
    check runTest("operator_negate")

  test "operator_not":
    check runTest("operator_not")

  test "operator_not_class":
    check runTest("operator_not_class")

  test "operator_not_equals":
    check runTest("operator_not_equals")

  test "operator_subtract":
    check runTest("operator_subtract")

  test "function_parameters":
    check runTest("function_parameters")

  test "function_recursion":
    check runTest("function_recursion")