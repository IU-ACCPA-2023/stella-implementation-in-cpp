cmake_minimum_required(VERSION 3.19.0)
project(stella-interpreter VERSION 0.1.0)

set (Stella_Syntax
  src/Stella/Absyn.H
  src/Stella/Absyn.C
  src/Stella/Parser.H
  src/Stella/Parser.C
  src/Stella/ParserError.H
  src/Stella/Printer.H
  src/Stella/Printer.C
  src/Stella/Buffer.H
  src/Stella/Buffer.C
  src/Stella/Bison.H
  src/Stella/Lexer.C
)

set (SRCS
  src/Interpret.cpp
  src/Eval.h
  src/Eval.cpp
  src/TypeCheck.h
  src/TypeCheck.cpp
  src/VisitTypeCheck.h
  src/VisitTypeCheck.cpp
)

add_executable(stella-interpreter ${Stella_Syntax} ${SRCS})
set_property(TARGET stella-interpreter PROPERTY CXX_STANDARD 17)

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
include(CPack)

include(CTest)

add_test(NAME added-test-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/added-test-1.stella)
add_test(NAME added-test-2 COMMAND stella-interpreter typecheck ../tests/ill-typed/added-test-2.stella)
add_test(NAME application-param-type COMMAND stella-interpreter typecheck ../tests/ill-typed/application-param-type.stella)
add_test(NAME applying-non-function-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/applying-non-function-1.stella)
add_test(NAME applying-non-function-2 COMMAND stella-interpreter typecheck ../tests/ill-typed/applying-non-function-2.stella)
add_test(NAME applying-non-function-3 COMMAND stella-interpreter typecheck ../tests/ill-typed/applying-non-function-3.stella)
add_test(NAME argument-type-mismatch-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/argument-type-mismatch-1.stella)
add_test(NAME argument-type-mismatch-2 COMMAND stella-interpreter typecheck ../tests/ill-typed/argument-type-mismatch-2.stella)
add_test(NAME argument-type-mismatch-3 COMMAND stella-interpreter typecheck ../tests/ill-typed/argument-type-mismatch-3.stella)
add_test(NAME bad-abstraction COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-abstraction.stella)
add_test(NAME bad-factorial-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-factorial-1.stella)
add_test(NAME bad-factorial-2 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-factorial-2.stella)
add_test(NAME bad-factorial COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-factorial.stella)
add_test(NAME bad-function-call COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-function-call.stella)
add_test(NAME bad-higher-order-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-higher-order-1.stella)
add_test(NAME bad-if-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-if-1.stella)
add_test(NAME bad-if-2 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-if-2.stella)
add_test(NAME bad-if-3 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-if-3.stella)
add_test(NAME bad-if-4 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-if-4.stella)
add_test(NAME bad-if-and-undefined-variable-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-if-and-undefined-variable-1.stella)
add_test(NAME bad-iszero COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-iszero.stella)
add_test(NAME bad-logic-not-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-logic-not-1.stella)
add_test(NAME bad-nat-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-nat-1.stella)
add_test(NAME bad-nat-2 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-nat-2.stella)
add_test(NAME bad-nat-rec-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-nat-rec-1.stella)
add_test(NAME bad-nat-rec-2 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-nat-rec-2.stella)
add_test(NAME bad-return-type COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-return-type.stella)
add_test(NAME bad-squares-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-squares-1.stella)
add_test(NAME bad-squares-2 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-squares-2.stella)
add_test(NAME bad-succ-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-succ-1.stella)
add_test(NAME bad-succ-2 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-succ-2.stella)
add_test(NAME bad-succ-3 COMMAND stella-interpreter typecheck ../tests/ill-typed/bad-succ-3.stella)
add_test(NAME function-mismatch COMMAND stella-interpreter typecheck ../tests/ill-typed/function-mismatch.stella)
add_test(NAME invalid-nat COMMAND stella-interpreter typecheck ../tests/ill-typed/invalid-nat.stella)
add_test(NAME invalid-not_ COMMAND stella-interpreter typecheck ../tests/ill-typed/invalid-not_.stella)
add_test(NAME my-factorial COMMAND stella-interpreter typecheck ../tests/ill-typed/my-factorial.stella)
add_test(NAME my-ill-test-2 COMMAND stella-interpreter typecheck ../tests/ill-typed/my-ill-test-2.stella)
add_test(NAME my-ill-typed-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/my-ill-typed-1.stella)
add_test(NAME my-ill-typed-2 COMMAND stella-interpreter typecheck ../tests/ill-typed/my-ill-typed-2.stella)
add_test(NAME my-mismatch COMMAND stella-interpreter typecheck ../tests/ill-typed/my-mismatch.stella)
add_test(NAME nat__rec-parameters COMMAND stella-interpreter typecheck ../tests/ill-typed/nat__rec-parameters.stella)
add_test(NAME shadowed-variable-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/shadowed-variable-1.stella)
add_test(NAME shadowed-variable-2 COMMAND stella-interpreter typecheck ../tests/ill-typed/shadowed-variable-2.stella)
add_test(NAME undefined-variable-1 COMMAND stella-interpreter typecheck ../tests/ill-typed/undefined-variable-1.stella)
add_test(NAME undefined-variable-2 COMMAND stella-interpreter typecheck ../tests/ill-typed/undefined-variable-2.stella)
add_test(NAME undefined-variable-3 COMMAND stella-interpreter typecheck ../tests/ill-typed/undefined-variable-3.stella)

set_tests_properties(
    added-test-1
    added-test-2
    application-param-type
    applying-non-function-1
    applying-non-function-2
    applying-non-function-3
    argument-type-mismatch-1
    argument-type-mismatch-2
    argument-type-mismatch-3
    bad-abstraction
    bad-factorial-1
    bad-factorial-2
    bad-factorial
    bad-function-call
    bad-higher-order-1
    bad-if-1
    bad-if-2
    bad-if-3
    bad-if-4
    bad-if-and-undefined-variable-1
    bad-iszero
    bad-logic-not-1
    bad-nat-1
    bad-nat-2
    bad-nat-rec-1
    bad-nat-rec-2
    bad-return-type
    bad-squares-1
    bad-squares-2
    bad-succ-1
    bad-succ-2
    bad-succ-3
    function-mismatch
    invalid-nat
    invalid-not_
    my-factorial
    my-ill-test-2
    my-ill-typed-1
    my-ill-typed-2
    my-mismatch
    nat__rec-parameters
    shadowed-variable-1
    shadowed-variable-2
    undefined-variable-1
    undefined-variable-2
    undefined-variable-3
  PROPERTIES WILL_FAIL TRUE)

add_test(NAME good-abstract-function COMMAND stella-interpreter typecheck ../tests/well-typed/abstract-function.stella)
add_test(NAME good-added-test-1 COMMAND stella-interpreter typecheck ../tests/well-typed/added-test-1.stella)
add_test(NAME good-added-test-2 COMMAND stella-interpreter typecheck ../tests/well-typed/added-test-2.stella)
add_test(NAME good-apply-increase COMMAND stella-interpreter typecheck ../tests/well-typed/apply-increase.stella)
add_test(NAME good-applying-actual-function-3 COMMAND stella-interpreter typecheck ../tests/well-typed/applying-actual-function-3.stella)
add_test(NAME good-bool-to-nat COMMAND stella-interpreter typecheck ../tests/well-typed/bool-to-nat.stella)
add_test(NAME good-cubes COMMAND stella-interpreter typecheck ../tests/well-typed/cubes.stella)
add_test(NAME good-double-application COMMAND stella-interpreter typecheck ../tests/well-typed/double-application.stella)
add_test(NAME good-factorial COMMAND stella-interpreter typecheck ../tests/well-typed/factorial.stella)
add_test(NAME good-good-if-2 COMMAND stella-interpreter typecheck ../tests/well-typed/good-if-2.stella)
add_test(NAME good-good-if COMMAND stella-interpreter typecheck ../tests/well-typed/good-if.stella)
add_test(NAME good-good-succ-1 COMMAND stella-interpreter typecheck ../tests/well-typed/good-succ-1.stella)
add_test(NAME good-good-succ-2 COMMAND stella-interpreter typecheck ../tests/well-typed/good-succ-2.stella)
add_test(NAME good-higher-order-1 COMMAND stella-interpreter typecheck ../tests/well-typed/higher-order-1.stella)
add_test(NAME good-higher-order-2 COMMAND stella-interpreter typecheck ../tests/well-typed/higher-order-2.stella)
add_test(NAME good-increment-triple COMMAND stella-interpreter typecheck ../tests/well-typed/increment-triple.stella)
add_test(NAME good-increment_twice COMMAND stella-interpreter typecheck ../tests/well-typed/increment_twice.stella)
add_test(NAME good-inner-if COMMAND stella-interpreter typecheck ../tests/well-typed/inner-if.stella)
add_test(NAME good-logical-operators COMMAND stella-interpreter typecheck ../tests/well-typed/logical-operators.stella)
add_test(NAME good-many-if COMMAND stella-interpreter typecheck ../tests/well-typed/many-if.stella)
add_test(NAME good-my-good-if COMMAND stella-interpreter typecheck ../tests/well-typed/my-good-if.stella)
add_test(NAME good-my-good-succ COMMAND stella-interpreter typecheck ../tests/well-typed/my-good-succ.stella)
add_test(NAME good-my-well-typed-1 COMMAND stella-interpreter typecheck ../tests/well-typed/my-well-typed-1.stella)
add_test(NAME good-my-well-typed-2 COMMAND stella-interpreter typecheck ../tests/well-typed/my-well-typed-2.stella)
add_test(NAME good-nat-to-bool COMMAND stella-interpreter typecheck ../tests/well-typed/nat-to-bool.stella)
add_test(NAME good-negate COMMAND stella-interpreter typecheck ../tests/well-typed/negate.stella)
add_test(NAME good-nested COMMAND stella-interpreter typecheck ../tests/well-typed/nested.stella)
add_test(NAME good-shadowed-variable-2 COMMAND stella-interpreter typecheck ../tests/well-typed/shadowed-variable-2.stella)
add_test(NAME good-simple-succ COMMAND stella-interpreter typecheck ../tests/well-typed/simple-succ.stella)
add_test(NAME good-simple-types COMMAND stella-interpreter typecheck ../tests/well-typed/simple-types.stella)
add_test(NAME good-squares COMMAND stella-interpreter typecheck ../tests/well-typed/squares.stella)
add_test(NAME good-succ-with-func COMMAND stella-interpreter typecheck ../tests/well-typed/succ-with-func.stella)
add_test(NAME good-x-simple-if COMMAND stella-interpreter typecheck ../tests/well-typed/x-simple-if.stella)
