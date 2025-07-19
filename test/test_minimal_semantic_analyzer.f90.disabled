program test_minimal_semantic_analyzer
    use ast_core
    use semantic_analyzer
    use type_system_hm
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_create_semantic_context()
    call test_analyze_single_assignment()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_create_semantic_context()
        type(semantic_context_t) :: ctx

        test_count = test_count + 1

        ! Create semantic context
        ctx = create_semantic_context()

        ! Check context is initialized
        if (ctx%next_var_id >= 0) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Create semantic context"
        else
            write (*, '(A)') "FAIL: Semantic context not initialized"
        end if
    end subroutine test_create_semantic_context

    subroutine test_analyze_single_assignment()
        type(semantic_context_t) :: ctx
        type(assignment_node) :: assign
        type(identifier_node) :: target
        type(literal_node) :: value
        type(mono_type_t) :: inferred_type

        test_count = test_count + 1

        ! Create semantic context
        ctx = create_semantic_context()

        ! Create a simple assignment: x = 42
        target%name = "x"
        target%line = 1
        target%column = 1

        value%value = "42"
        value%literal_kind = LITERAL_INTEGER
        value%line = 1
        value%column = 5

        assign%line = 1
        assign%column = 1
        allocate (assign%target, source=target)
        allocate (assign%value, source=value)

        ! Analyze the assignment
        inferred_type = ctx%infer_stmt(assign)

        ! Check if type inference worked
        if (inferred_type%kind == TINT) then
            ! Check if target got inferred type
            select type (target_ref => assign%target)
            type is (identifier_node)
                if (allocated(target_ref%inferred_type)) then
                    if (target_ref%inferred_type%kind == TINT) then
                        pass_count = pass_count + 1
                        write (*, '(A)') "PASS: Analyze single assignment"
                    else
                        write (*, '(A)') "FAIL: Target inferred type wrong kind"
                    end if
                else
                    write (*, '(A)') "FAIL: Target inferred type not set"
                end if
            class default
                write (*, '(A)') "FAIL: Assignment target wrong type"
            end select
        else
            write (*, '(A)') "FAIL: Assignment inferred type wrong"
        end if
    end subroutine test_analyze_single_assignment

end program test_minimal_semantic_analyzer
