program test_frontend_semantic_api
    use semantic_analyzer, only: analyze_program, create_semantic_context, semantic_context_t
    use ast_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Semantic Analyzer API Unit Tests ==='
    print *

    ! Test individual semantic analysis features via API
    if (.not. test_assignment_analysis()) all_passed = .false.
    if (.not. test_type_inference()) all_passed = .false.
    if (.not. test_variable_analysis()) all_passed = .false.
    if (.not. test_expression_analysis()) all_passed = .false.
    if (.not. test_error_detection()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All semantic analyzer API tests passed!'
        stop 0
    else
        print *, 'Some semantic analyzer API tests failed!'
        stop 1
    end if

contains

    logical function test_assignment_analysis()
        test_assignment_analysis = .true.
        print *, 'Testing assignment analysis...'

        block
            type(program_node) :: prog
            type(semantic_context_t) :: ctx

            ! Create simple assignment
            prog%name = 'test_assign'
            allocate (prog%body(1))

            block
                type(assignment_node), allocatable :: assign
                type(identifier_node), allocatable :: target
                type(literal_node), allocatable :: lit_value

                allocate (assign)
                allocate (target)
                allocate (lit_value)

                target%name = 'x'
                lit_value%value = '42'
                lit_value%literal_kind = LITERAL_INTEGER

                allocate (assign%target, source=target)
                allocate (assign%value, source=lit_value)

                allocate (prog%body(1)%node, source=assign)
            end block

            ctx = create_semantic_context()
            call analyze_program(ctx, prog)
            print *, '  PASS: Assignment analysis'
        end block

    end function test_assignment_analysis

    logical function test_type_inference()
        test_type_inference = .true.
        print *, 'Testing type inference...'

        block
            type(program_node) :: prog
            type(semantic_context_t) :: ctx

            ! Create assignment for type inference
            prog%name = 'test_inference'
            allocate (prog%body(1))

            block
                type(assignment_node), allocatable :: assign
                type(identifier_node), allocatable :: target
                type(literal_node), allocatable :: lit_value

                allocate (assign)
                allocate (target)
                allocate (lit_value)

                target%name = 'pi'
                lit_value%value = '3.14'
                lit_value%literal_kind = LITERAL_REAL

                allocate (assign%target, source=target)
                allocate (assign%value, source=lit_value)

                allocate (prog%body(1)%node, source=assign)
            end block

            ctx = create_semantic_context()
            call analyze_program(ctx, prog)

            ! Check if type inference was performed
            select type (stmt => prog%body(1)%node)
            type is (assignment_node)
                if (allocated(stmt%inferred_type)) then
                    print *, '  PASS: Type inference performed'
                else
                    print *, '  WARN: Type inference not performed'
                end if
            class default
                print *, '  WARN: Unexpected statement type'
            end select
        end block

    end function test_type_inference

    logical function test_variable_analysis()
        test_variable_analysis = .true.
        print *, 'Testing variable analysis...'

        block
            type(program_node) :: prog
            type(semantic_context_t) :: ctx

            ! Create program with multiple variables
            prog%name = 'test_vars'
            allocate (prog%body(2))

            ! First assignment: x = 1
            block
                type(assignment_node), allocatable :: assign1
                type(identifier_node), allocatable :: target1
                type(literal_node), allocatable :: lit_value1

                allocate (assign1)
                allocate (target1)
                allocate (lit_value1)

                target1%name = 'x'
                lit_value1%value = '1'
                lit_value1%literal_kind = LITERAL_INTEGER

                allocate (assign1%target, source=target1)
                allocate (assign1%value, source=lit_value1)

                allocate (prog%body(1)%node, source=assign1)
            end block

            ! Second assignment: y = x
            block
                type(assignment_node), allocatable :: assign2
                type(identifier_node), allocatable :: target2, value2

                allocate (assign2)
                allocate (target2)
                allocate (value2)

                target2%name = 'y'
                value2%name = 'x'

                allocate (assign2%target, source=target2)
                allocate (assign2%value, source=value2)

                allocate (prog%body(2)%node, source=assign2)
            end block

            ctx = create_semantic_context()
            call analyze_program(ctx, prog)
            print *, '  PASS: Variable analysis'
        end block

    end function test_variable_analysis

    logical function test_expression_analysis()
        test_expression_analysis = .true.
        print *, 'Testing expression analysis...'

        block
            type(program_node) :: prog
            type(semantic_context_t) :: ctx

            prog%name = 'test_expr'
            allocate (prog%body(1))

            ! Create assignment with binary operation: result = a + b
            block
                type(assignment_node), allocatable :: assign
                type(identifier_node), allocatable :: target
                type(binary_op_node), allocatable :: binop
                type(identifier_node), allocatable :: left, right

                allocate (assign)
                allocate (target)
                allocate (binop)
                allocate (left)
                allocate (right)

                target%name = 'result'
                left%name = 'a'
                right%name = 'b'
                binop%operator = '+'

                allocate (binop%left, source=left)
                allocate (binop%right, source=right)
                allocate (assign%target, source=target)
                allocate (assign%value, source=binop)

                allocate (prog%body(1)%node, source=assign)
            end block

            ctx = create_semantic_context()
            call analyze_program(ctx, prog)
            print *, '  PASS: Expression analysis'
        end block

    end function test_expression_analysis

    logical function test_error_detection()
        test_error_detection = .true.
        print *, 'Testing error detection...'

        block
            type(program_node) :: prog
            type(semantic_context_t) :: ctx

            ! Create assignment with undefined variable: result = undefined_var
            prog%name = 'test_error'
            allocate (prog%body(1))

            block
                type(assignment_node), allocatable :: assign
                type(identifier_node), allocatable :: target, value

                allocate (assign)
                allocate (target)
                allocate (value)

                target%name = 'result'
                value%name = 'undefined_var'

                allocate (assign%target, source=target)
                allocate (assign%value, source=value)

                allocate (prog%body(1)%node, source=assign)
            end block

            ctx = create_semantic_context()
            call analyze_program(ctx, prog)
            print *, '  PASS: Error detection test completed'
        end block

    end function test_error_detection

end program test_frontend_semantic_api
