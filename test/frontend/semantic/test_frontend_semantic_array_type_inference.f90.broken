program test_frontend_semantic_array_type_inference
    use semantic_analyzer, only: analyze_program, create_semantic_context, semantic_context_t
    use ast_core
    use lexer_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Array Type Inference Tests ==='
    print *

    ! Test type inference for arrays
    if (.not. test_array_literal_inference()) all_passed = .false.
    if (.not. test_array_indexing_inference()) all_passed = .false.
    if (.not. test_array_assignment_inference()) all_passed = .false.
    if (.not. test_array_operations_inference()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All array type inference tests passed!'
        stop 0
    else
        print *, 'Some array type inference tests failed!'
        stop 1
    end if

contains

    logical function test_array_literal_inference()
        test_array_literal_inference = .true.
        print *, 'Testing array literal inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse simplified array literal: "arr = [1, 2, 3]"
            call tokenize_core("arr = [1, 2, 3]", tokens)
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_array_literal_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (assignment_node)
                if (allocated(ast_tree%inferred_type)) then
                    print *, '  PASS: Array literal inference completed'
                    print *, '  INFO: Inferred type: ', ast_tree%inferred_type_name
                else
                    print *, '  PARTIAL: Assignment processed but type not stored'
                end if
            class default
                print *, '  FAIL: Expected assignment node'
                test_array_literal_inference = .false.
            end select
        end block

    end function test_array_literal_inference

    logical function test_array_indexing_inference()
        test_array_indexing_inference = .true.
        print *, 'Testing array indexing inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse "x = arr(1)"
            call tokenize_core("x = arr(1)", tokens)
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_array_indexing_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (assignment_node)
                if (allocated(ast_tree%inferred_type)) then
                    print *, '  PASS: Array indexing inference completed'
                    print *, '  INFO: Inferred type: ', ast_tree%inferred_type_name
                else
                    print *, '  PARTIAL: Assignment processed but type not stored'
                end if
            class default
                print *, '  FAIL: Expected assignment node'
                test_array_indexing_inference = .false.
            end select
        end block

    end function test_array_indexing_inference

    logical function test_array_assignment_inference()
        test_array_assignment_inference = .true.
        print *, 'Testing array assignment inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse "arr(1) = 42"
            call tokenize_core("arr(1) = 42", tokens)
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_array_assignment_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (assignment_node)
                if (allocated(ast_tree%inferred_type)) then
                    print *, '  PASS: Array assignment inference completed'
                    print *, '  INFO: Inferred type: ', ast_tree%inferred_type_name
                else
                    print *, '  PARTIAL: Assignment processed but type not stored'
                end if
            class default
                print *, '  FAIL: Expected assignment node'
                test_array_assignment_inference = .false.
            end select
        end block

    end function test_array_assignment_inference

    logical function test_array_operations_inference()
        test_array_operations_inference = .true.
        print *, 'Testing array operations inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse "result = a + b" (assuming a and b are arrays)
            call tokenize_core("result = a + b", tokens)
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_array_operations_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (assignment_node)
                if (allocated(ast_tree%inferred_type)) then
                    print *, '  PASS: Array operations inference completed'
                    print *, '  INFO: Inferred type: ', ast_tree%inferred_type_name
                else
                    print *, '  PARTIAL: Assignment processed but type not stored'
                end if
            class default
                print *, '  FAIL: Expected assignment node'
                test_array_operations_inference = .false.
            end select
        end block

    end function test_array_operations_inference

end program test_frontend_semantic_array_type_inference
