program test_scope_and_builtins
    use semantic_analyzer
    use type_system_hm
    use ast_core
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0

    write (*, '(a)') '=== Testing Scope and Builtin Functions ==='
    write (*, '(a)') ''

    call test_identifier_lookup()
    call test_builtin_functions()
    call test_type_storage()

    write (*, '(a)') ''
    write(*, '(a,i0,a,i0,a)') 'Scope and builtin tests: ', pass_count, '/', test_count, ' passed'

    if (pass_count /= test_count) then
        error stop 'Some tests failed!'
    end if

contains

    subroutine test_identifier_lookup()
        type(semantic_context_t) :: ctx
        type(assignment_node) :: assign
        type(identifier_node) :: target, value_ident
        type(literal_node) :: value_lit
        type(mono_type_t) :: result_type

        write (*, '(a)') 'Test 1: Identifier lookup in environment'
        test_count = test_count + 1

        ctx = create_semantic_context()

        ! Create assignment: x = 42
        target = create_identifier("x", 1, 1)
        value_lit = create_literal("42", LITERAL_INTEGER, 1, 5)
        assign = create_assignment(target, value_lit, 1, 1)

        ! Infer type and add to environment
        result_type = ctx%infer_stmt(assign)

        ! Now create identifier reference to x
        value_ident = create_identifier("x", 2, 1)
        result_type = ctx%infer(value_ident)

        ! Check that x has integer type (not a fresh type variable)
        if (result_type%kind == TINT) then
            pass_count = pass_count + 1
            write (*, '(a)') '  ✓ PASS: Identifier lookup works correctly'
        else
            write (*, '(a)') '  ✗ FAIL: Identifier lookup failed'
            write (*, '(a,a)') '    Expected: integer, Got: ', result_type%to_string()
        end if

    end subroutine test_identifier_lookup

    subroutine test_builtin_functions()
        type(semantic_context_t) :: ctx
        type(function_call_node) :: call_node
        type(literal_node) :: arg
        type(ast_node_wrapper) :: arg_wrapper
        type(mono_type_t) :: result_type

        write (*, '(a)') 'Test 2: Builtin function types'
        test_count = test_count + 1

        ctx = create_semantic_context()

        ! Create function call: sin(3.14)
        arg = create_literal("3.14", LITERAL_REAL, 1, 5)
        allocate (arg_wrapper%node, source=arg)
        call_node = create_function_call("sin", [arg_wrapper], 1, 1)

        ! Infer type
        result_type = ctx%infer(call_node)

        ! Check that result is real
        if (result_type%kind == TREAL) then
            pass_count = pass_count + 1
            write (*, '(a)') '  ✓ PASS: Builtin function types work correctly'
        else
            write (*, '(a)') '  ✗ FAIL: Builtin function type inference failed'
            write (*, '(a,a)') '    Expected: real, Got: ', result_type%to_string()
        end if

    end subroutine test_builtin_functions

    subroutine test_type_storage()
        type(semantic_context_t) :: ctx
        type(assignment_node) :: assign
        type(identifier_node) :: target
        type(literal_node) :: value
        type(mono_type_t) :: result_type

        write (*, '(a)') 'Test 3: Type storage in AST nodes'
        test_count = test_count + 1

        ctx = create_semantic_context()

        ! Create assignment: pi = 3.14159
        target = create_identifier("pi", 1, 1)
        value = create_literal("3.14159", LITERAL_REAL, 1, 5)
        assign = create_assignment(target, value, 1, 1)

        ! Analyze and store types
        call analyze_program(ctx, assign)

        ! Check that types are stored
        if (allocated(assign%inferred_type) .and. &
            allocated(target%inferred_type) .and. &
            allocated(value%inferred_type)) then

            if (assign%inferred_type%kind == TREAL .and. &
                target%inferred_type%kind == TREAL .and. &
                value%inferred_type%kind == TREAL) then
                pass_count = pass_count + 1
                write (*, '(a)') '  ✓ PASS: Types correctly stored in AST nodes'
            else
                write (*, '(a)') '  ✗ FAIL: Incorrect types stored'
            end if
        else
            write (*, '(a)') '  ✗ FAIL: Types not stored in AST nodes'
        end if

    end subroutine test_type_storage

end program test_scope_and_builtins
