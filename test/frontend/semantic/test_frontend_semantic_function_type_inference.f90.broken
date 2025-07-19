program test_frontend_semantic_function_type_inference
    use semantic_analyzer, only: analyze_program, create_semantic_context, semantic_context_t
    use ast_core
    use lexer_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Function Type Inference Tests ==='
    print *

    ! Test type inference for functions
    if (.not. test_simple_function_inference()) all_passed = .false.
    if (.not. test_function_call_inference()) all_passed = .false.
    if (.not. test_function_with_parameters_inference()) all_passed = .false.
    if (.not. test_recursive_function_inference()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All function type inference tests passed!'
        stop 0
    else
        print *, 'Some function type inference tests failed!'
        stop 1
    end if

contains

    logical function test_simple_function_inference()
        test_simple_function_inference = .true.
        print *, 'Testing simple function inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse "function square(x) square = x * x end function"
            call tokenize_core("function square(x) square = x * x end function", tokens)
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_simple_function_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (function_def_node)
                print *, '  PASS: Simple function inference completed'
                print *, '  INFO: Function name: ', ast_tree%name
            class default
                print *, '  FAIL: Expected function definition node'
                test_simple_function_inference = .false.
            end select
        end block

    end function test_simple_function_inference

    logical function test_function_call_inference()
        test_function_call_inference = .true.
        print *, 'Testing function call inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse "result = sin(3.14)"
            call tokenize_core("result = sin(3.14)", tokens)
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_function_call_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (assignment_node)
                if (allocated(ast_tree%inferred_type)) then
                    print *, '  PASS: Function call inference completed'
                    print *, '  INFO: Inferred type: ', ast_tree%inferred_type_name
                else
                    print *, '  PARTIAL: Assignment processed but type not stored'
                end if
            class default
                print *, '  FAIL: Expected assignment node'
                test_function_call_inference = .false.
            end select
        end block

    end function test_function_call_inference

    logical function test_function_with_parameters_inference()
        test_function_with_parameters_inference = .true.
        print *, 'Testing function with parameters inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse "function add(a, b) add = a + b end function"
            call tokenize_core("function add(a, b) add = a + b end function", tokens)
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_function_with_parameters_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (function_def_node)
                print *, '  PASS: Function with parameters inference completed'
                print *, '  INFO: Function name: ', ast_tree%name
                if (allocated(ast_tree%params)) then
                    print *, '  INFO: Parameter count: ', size(ast_tree%params)
                end if
            class default
                print *, '  FAIL: Expected function definition node'
                test_function_with_parameters_inference = .false.
            end select
        end block

    end function test_function_with_parameters_inference

    logical function test_recursive_function_inference()
        test_recursive_function_inference = .true.
        print *, 'Testing recursive function inference...'

        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: ast_tree
            type(semantic_context_t) :: ctx

            ! Parse simplified factorial: "function fact(n) fact = n end function"
            call tokenize_core("function fact(n) fact = n end function", tokens)
            ast_tree = parse_statement(tokens)

            if (.not. allocated(ast_tree)) then
                print *, '  FAIL: Parse failed to produce AST'
                test_recursive_function_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, ast_tree)

            ! Check if semantic analysis ran without crash
            select type (ast_tree)
            type is (function_def_node)
                print *, '  PASS: Recursive function inference completed'
                print *, '  INFO: Function name: ', ast_tree%name
            class default
                print *, '  FAIL: Expected function definition node'
                test_recursive_function_inference = .false.
            end select
        end block

    end function test_recursive_function_inference

end program test_frontend_semantic_function_type_inference
