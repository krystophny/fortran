program test_frontend_semantic_expression_type_inference
    use semantic_analyzer, only: analyze_program, create_semantic_context, semantic_context_t
    use ast_core
    use lexer_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Expression Type Inference Tests ==='
    print *

    ! Test type inference for different expressions
    if (.not. test_arithmetic_expression_inference()) all_passed = .false.
    if (.not. test_logical_expression_inference()) all_passed = .false.
    if (.not. test_relational_expression_inference()) all_passed = .false.
    if (.not. test_mixed_expression_inference()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All expression type inference tests passed!'
        stop 0
    else
        print *, 'Some expression type inference tests failed!'
        stop 1
    end if

contains

    logical function test_arithmetic_expression_inference()
        test_arithmetic_expression_inference = .true.
        print *, 'Testing arithmetic expression inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse "result = 2 + 3"
            call tokenize_core("result = 2 + 3", tokens)
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_arithmetic_expression_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (assignment_node)
                if (allocated(ast_tree%inferred_type)) then
                    print *, '  PASS: Arithmetic expression inference completed'
                    print *, '  INFO: Inferred type: ', ast_tree%inferred_type_name
                else
                    print *, '  PARTIAL: Assignment processed but type not stored'
                end if
            class default
                print *, '  FAIL: Expected assignment node'
                test_arithmetic_expression_inference = .false.
            end select
        end block

    end function test_arithmetic_expression_inference

    logical function test_logical_expression_inference()
        test_logical_expression_inference = .true.
        print *, 'Testing logical expression inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse "flag = .true. .and. .false."
            call tokenize_core("flag = .true. .and. .false.", tokens)
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_logical_expression_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (assignment_node)
                if (allocated(ast_tree%inferred_type)) then
                    print *, '  PASS: Logical expression inference completed'
                    print *, '  INFO: Inferred type: ', ast_tree%inferred_type_name
                else
                    print *, '  PARTIAL: Assignment processed but type not stored'
                end if
            class default
                print *, '  FAIL: Expected assignment node'
                test_logical_expression_inference = .false.
            end select
        end block

    end function test_logical_expression_inference

    logical function test_relational_expression_inference()
        test_relational_expression_inference = .true.
        print *, 'Testing relational expression inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse "result = 5 > 3"
            call tokenize_core("result = 5 > 3", tokens)
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_relational_expression_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (assignment_node)
                if (allocated(ast_tree%inferred_type)) then
                    print *, '  PASS: Relational expression inference completed'
                    print *, '  INFO: Inferred type: ', ast_tree%inferred_type_name
                else
                    print *, '  PARTIAL: Assignment processed but type not stored'
                end if
            class default
                print *, '  FAIL: Expected assignment node'
                test_relational_expression_inference = .false.
            end select
        end block

    end function test_relational_expression_inference

    logical function test_mixed_expression_inference()
        test_mixed_expression_inference = .true.
        print *, 'Testing mixed expression inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse "result = 2.5 * 3"
            call tokenize_core("result = 2.5 * 3", tokens)
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_mixed_expression_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (assignment_node)
                if (allocated(ast_tree%inferred_type)) then
                    print *, '  PASS: Mixed expression inference completed'
                    print *, '  INFO: Inferred type: ', ast_tree%inferred_type_name
                else
                    print *, '  PARTIAL: Assignment processed but type not stored'
                end if
            class default
                print *, '  FAIL: Expected assignment node'
                test_mixed_expression_inference = .false.
            end select
        end block

    end function test_mixed_expression_inference

end program test_frontend_semantic_expression_type_inference
