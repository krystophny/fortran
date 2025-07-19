program test_ast_json_io
    ! Test AST JSON serialization and deserialization
    use ast_core
    use json_writer, only: json_write_ast_to_file, json_write_ast_to_string
    use json_reader, only: json_read_ast_from_file
    implicit none

    integer :: num_tests, num_passed
    character(len=256) :: temp_file

    num_tests = 0
    num_passed = 0

    temp_file = "/tmp/test_ast_json.json"

    ! Test 1: Simple assignment AST
    call test_assignment_roundtrip(num_tests, num_passed)

    ! Test 2: Binary operation AST
    call test_binary_op_roundtrip(num_tests, num_passed)

    ! Test 3: Function definition AST
    call test_function_def_roundtrip(num_tests, num_passed)

    ! Test 4: Program with multiple statements
    call test_program_roundtrip(num_tests, num_passed)

    ! Summary
    print *, "================================"
    print *, "AST JSON I/O Test Summary"
    print *, "================================"
    print '(a,i0,a,i0)', "Passed: ", num_passed, " / ", num_tests

    if (num_passed /= num_tests) then
        error stop "AST JSON I/O tests failed!"
    end if

contains

    subroutine test_assignment_roundtrip(num_tests, num_passed)
        integer, intent(inout) :: num_tests, num_passed
        type(assignment_node) :: ast1
        class(ast_node), allocatable :: ast2
        type(identifier_node) :: target
        type(literal_node) :: value
        character(len=:), allocatable :: json1, json2

        num_tests = num_tests + 1
        print *, "Test: Assignment round-trip"

        ! Create assignment AST: x = 42
        target = create_identifier("x", 1, 1)
        value = create_literal("42", LITERAL_INTEGER, 1, 5)
        ast1 = create_assignment(target, value, 1, 1)

        ! Serialize to JSON
        json1 = json_write_ast_to_string(ast1)
        print *, "  Original JSON: ", trim(json1)

        ! Write to file
        call json_write_ast_to_file(ast1, temp_file)

        ! Read back
        ast2 = json_read_ast_from_file(temp_file)

        ! Serialize again
        json2 = json_write_ast_to_string(ast2)
        print *, "  Restored JSON: ", trim(json2)

        ! Compare
        if (json1 == json2) then
            print *, "  PASS"
            num_passed = num_passed + 1
        else
            print *, "  FAIL: JSON mismatch"
        end if

        ! Clean up
        call execute_command_line("rm -f "//temp_file)

    end subroutine test_assignment_roundtrip

    subroutine test_binary_op_roundtrip(num_tests, num_passed)
        integer, intent(inout) :: num_tests, num_passed
        type(binary_op_node) :: ast1
        class(ast_node), allocatable :: ast2
        type(identifier_node) :: left
        type(literal_node) :: right
        character(len=:), allocatable :: json1, json2

        num_tests = num_tests + 1
        print *, "Test: Binary operation round-trip"

        ! Create binary op AST: x + 5
        left = create_identifier("x", 1, 1)
        right = create_literal("5", LITERAL_INTEGER, 1, 5)
        ast1 = create_binary_op(left, right, "+", 1, 3)

        ! Serialize to JSON
        json1 = json_write_ast_to_string(ast1)
        print *, "  Original JSON: ", trim(json1)

        ! Write to file
        call json_write_ast_to_file(ast1, temp_file)

        ! Read back
        ast2 = json_read_ast_from_file(temp_file)

        ! Serialize again
        json2 = json_write_ast_to_string(ast2)
        print *, "  Restored JSON: ", trim(json2)

        ! Compare
        if (json1 == json2) then
            print *, "  PASS"
            num_passed = num_passed + 1
        else
            print *, "  FAIL: JSON mismatch"
        end if

        ! Clean up
        call execute_command_line("rm -f "//temp_file)

    end subroutine test_binary_op_roundtrip

    subroutine test_function_def_roundtrip(num_tests, num_passed)
        integer, intent(inout) :: num_tests, num_passed
        type(function_def_node) :: ast1
        class(ast_node), allocatable :: ast2
        type(ast_node_wrapper), allocatable :: params(:), body(:)
        type(identifier_node) :: param
        type(assignment_node) :: stmt
        character(len=:), allocatable :: json1, json2

        num_tests = num_tests + 1
        print *, "Test: Function definition round-trip"

        ! Create function AST: function square(x)
        allocate (params(1))
        param = create_identifier("x", 1, 18)
        allocate (params(1)%node, source=param)

        allocate (body(1))
        ! square = x * x
        block
            type(identifier_node) :: target, x1, x2
            type(binary_op_node) :: expr
            target = create_identifier("square", 2, 3)
            x1 = create_identifier("x", 2, 11)
            x2 = create_identifier("x", 2, 15)
            expr = create_binary_op(x1, x2, "*", 2, 13)
            stmt = create_assignment(target, expr, 2, 3)
        end block
        allocate (body(1)%node, source=stmt)

        block
            type(identifier_node) :: return_type
            return_type = create_identifier("real", 1, 1)
            ast1 = create_function_def("square", params, return_type, body, 1, 1)
        end block

        ! Serialize to JSON
        json1 = json_write_ast_to_string(ast1)
        print *, "  Original JSON: ", trim(json1)

        ! Write to file
        call json_write_ast_to_file(ast1, temp_file)

        ! Read back
        ast2 = json_read_ast_from_file(temp_file)

        ! Serialize again
        json2 = json_write_ast_to_string(ast2)
        print *, "  Restored JSON: ", trim(json2)

        ! Compare
        if (json1 == json2) then
            print *, "  PASS"
            num_passed = num_passed + 1
        else
            print *, "  FAIL: JSON mismatch"
        end if

        ! Clean up
        call execute_command_line("rm -f "//temp_file)

    end subroutine test_function_def_roundtrip

    subroutine test_program_roundtrip(num_tests, num_passed)
        integer, intent(inout) :: num_tests, num_passed
        type(program_node) :: ast1
        class(ast_node), allocatable :: ast2
        character(len=:), allocatable :: json1, json2

        num_tests = num_tests + 1
        print *, "Test: Program round-trip"

        ! Create program manually using wrapper pattern
        ast1%name = "test"
        ast1%line = 1
        ast1%column = 1
        allocate (ast1%body(2))

        ! x = 1
        block
            type(identifier_node) :: target
            type(literal_node) :: value
            type(assignment_node) :: stmt
            target = create_identifier("x", 1, 1)
            value = create_literal("1", LITERAL_INTEGER, 1, 5)
            stmt = create_assignment(target, value, 1, 1)
            allocate (ast1%body(1)%node, source=stmt)
        end block

        ! print *, x
        block
            type(ast_node_wrapper), allocatable :: args(:)
            type(identifier_node) :: x
            type(print_statement_node) :: stmt
            allocate (args(1))
            x = create_identifier("x", 2, 10)
            allocate (args(1)%node, source=x)
            stmt = create_print_statement(args, "*", 2, 1)
            allocate (ast1%body(2)%node, source=stmt)
        end block

        ! Serialize to JSON
        json1 = json_write_ast_to_string(ast1)
        print *, "  Original JSON: ", trim(json1)

        ! Write to file
        call json_write_ast_to_file(ast1, temp_file)

        ! Read back
        ast2 = json_read_ast_from_file(temp_file)

        ! Serialize again
        json2 = json_write_ast_to_string(ast2)
        print *, "  Restored JSON: ", trim(json2)

        ! Compare
        if (json1 == json2) then
            print *, "  PASS"
            num_passed = num_passed + 1
        else
            print *, "  FAIL: JSON mismatch"
        end if

        ! Clean up
        call execute_command_line("rm -f "//temp_file)

    end subroutine test_program_roundtrip

end program test_ast_json_io
