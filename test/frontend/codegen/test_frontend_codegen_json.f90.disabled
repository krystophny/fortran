program test_frontend_codegen_json
    use json_module
    use ast_core
    use codegen_core, only: generate_code_polymorphic
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    ! Test 1: Generate code from JSON AST representation
    call test_codegen_from_json_ast()

    if (all_tests_passed) then
        print *, "All codegen JSON tests passed!"
        stop 0
    else
        print *, "Some codegen JSON tests failed!"
        stop 1
    end if

contains

    subroutine test_codegen_from_json_ast()
        ! Test code generation by manually creating AST nodes
        ! This approach simulates JSON-to-AST deserialization for testing
        type(assignment_node) :: assign
        class(ast_node), allocatable :: target, value
        character(len=:), allocatable :: code

        print *, "Testing: Code generation from AST (simulated JSON input)"

        ! Create AST nodes (simulating what would come from JSON)
        target = create_identifier("y", 1, 1)
        value = create_literal("3.14", LITERAL_REAL, 1, 5)
        assign = create_assignment(target, value, 1, 1)

        ! Generate code
        code = generate_code_polymorphic(assign)

        ! Check result
        if (code == "y = 3.14d0") then
            print *, "PASS: Generated code from 'AST': '", code, "'"
        else
            print *, "FAIL: Expected 'y = 3.14d0', got '", code, "'"
            all_tests_passed = .false.
        end if

        print *, "NOTE: Full JSON->AST deserialization still needs implementation"

    end subroutine test_codegen_from_json_ast

end program test_frontend_codegen_json
