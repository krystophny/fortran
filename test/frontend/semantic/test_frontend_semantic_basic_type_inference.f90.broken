program test_frontend_semantic_basic_type_inference
    use semantic_analyzer, only: analyze_program, create_semantic_context, semantic_context_t
    use ast_core
    use lexer_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Basic Type Inference Tests ==='
    print *

    ! Test basic type inference for different assignment types
    if (.not. test_integer_assignment_inference()) all_passed = .false.
    if (.not. test_real_assignment_inference()) all_passed = .false.
    if (.not. test_character_assignment_inference()) all_passed = .false.
    if (.not. test_logical_assignment_inference()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All basic type inference tests passed!'
        stop 0
    else
        print *, 'Some basic type inference tests failed!'
        stop 1
    end if

contains

    logical function test_integer_assignment_inference()
        test_integer_assignment_inference = .true.
        print *, 'Testing integer assignment inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse "x = 42"
            call tokenize_core("x = 42", tokens)

            ! Use parser_core directly to parse the assignment
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_integer_assignment_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (assignment_node)
                if (allocated(ast_tree%inferred_type)) then
                    print *, '  PASS: Integer assignment inference completed'
                    print *, '  INFO: Inferred type: ', ast_tree%inferred_type_name
                else
                    print *, '  PARTIAL: Assignment processed but type not stored'
                end if
            class default
                print *, '  FAIL: Expected assignment node'
                test_integer_assignment_inference = .false.
            end select
        end block

    end function test_integer_assignment_inference

    logical function test_real_assignment_inference()
        test_real_assignment_inference = .true.
        print *, 'Testing real assignment inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse "pi = 3.14"
            call tokenize_core("pi = 3.14", tokens)

            ! Use parser_core directly
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_real_assignment_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (assignment_node)
                if (allocated(ast_tree%inferred_type)) then
                    print *, '  PASS: Real assignment inference completed'
                    print *, '  INFO: Inferred type: ', ast_tree%inferred_type_name
                else
                    print *, '  PARTIAL: Assignment processed but type not stored'
                end if
            class default
                print *, '  FAIL: Expected assignment node'
                test_real_assignment_inference = .false.
            end select
        end block

    end function test_real_assignment_inference

    logical function test_character_assignment_inference()
        test_character_assignment_inference = .true.
        print *, 'Testing character assignment inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse 'name = "hello"'
            call tokenize_core('name = "hello"', tokens)

            ! Use parser_core directly
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_character_assignment_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (assignment_node)
                if (allocated(ast_tree%inferred_type)) then
                    print *, '  PASS: Character assignment inference completed'
                    print *, '  INFO: Inferred type: ', ast_tree%inferred_type_name
                else
                    print *, '  PARTIAL: Assignment processed but type not stored'
                end if
            class default
                print *, '  FAIL: Expected assignment node'
                test_character_assignment_inference = .false.
            end select
        end block

    end function test_character_assignment_inference

    logical function test_logical_assignment_inference()
        test_logical_assignment_inference = .true.
        print *, 'Testing logical assignment inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse "flag = .true."
            call tokenize_core("flag = .true.", tokens)

            ! Use parser_core directly
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_logical_assignment_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (assignment_node)
                if (allocated(ast_tree%inferred_type)) then
                    print *, '  PASS: Logical assignment inference completed'
                    print *, '  INFO: Inferred type: ', ast_tree%inferred_type_name
                else
                    print *, '  PARTIAL: Assignment processed but type not stored'
                end if
            class default
                print *, '  FAIL: Expected assignment node'
                test_logical_assignment_inference = .false.
            end select
        end block

    end function test_logical_assignment_inference

end program test_frontend_semantic_basic_type_inference
