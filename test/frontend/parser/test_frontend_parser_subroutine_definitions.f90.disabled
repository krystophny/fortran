program test_frontend_parser_subroutine_definitions
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for subroutine definitions
    if (.not. test_simple_subroutine_definition()) all_passed = .false.
    if (.not. test_subroutine_with_parameters()) all_passed = .false.
    if (.not. test_subroutine_no_parameters()) all_passed = .false.
    if (.not. test_subroutine_with_intent()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All subroutine definition parser tests passed"
        stop 0
    else
        print '(a)', "Some subroutine definition parser tests failed"
        stop 1
    end if

contains

    logical function test_simple_subroutine_definition()
        ! TDD Test 1: Parse simple subroutine definition
        ! subroutine swap(a, b)
        !     temp = a
        !     a = b
        !     b = temp
        ! end subroutine
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_simple_subroutine_definition = .true.

        print '(a)', "Testing simple subroutine definition..."

        ! Test: subroutine swap(a, b) end subroutine
        call tokenize_core("subroutine swap(a, b) end subroutine", tokens)

        ! Add EOF token
        block
            type(token_t), allocatable :: tokens_with_eof(:)
            integer :: n

            n = size(tokens)
            allocate (tokens_with_eof(n + 1))
            tokens_with_eof(1:n) = tokens
            tokens_with_eof(n + 1)%kind = TK_EOF
            tokens_with_eof(n + 1)%text = ""
            tokens_with_eof(n + 1)%line = 1
            tokens_with_eof(n + 1)%column = 1

            stmt = parse_statement(tokens_with_eof)
        end block

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: No AST node returned for subroutine definition"
            test_simple_subroutine_definition = .false.
        else
            ! Check what type of node we get
            select type (stmt)
            type is (subroutine_def_node)
                print '(a)', "PASS: Subroutine definition parsed as subroutine_def_node"

                ! Check subroutine name
                if (allocated(stmt%name)) then
                    if (stmt%name == "swap") then
                        print '(a)', "PASS: Subroutine name is 'swap'"
                    else
                        print '(a)', "FAIL: Wrong subroutine name"
                        test_simple_subroutine_definition = .false.
                    end if
                else
                    print '(a)', "FAIL: No subroutine name"
                    test_simple_subroutine_definition = .false.
                end if

                ! Check parameters
                if (.not. allocated(stmt%params)) then
                    print '(a)', "FAIL: No parameters in subroutine definition"
                    test_simple_subroutine_definition = .false.
                else
                    if (size(stmt%params) == 2) then
                        print '(a)', "PASS: Subroutine has 2 parameters"

                        ! Check first parameter
                        if (allocated(stmt%params(1)%node)) then
                            select type (param1 => stmt%params(1)%node)
                            type is (identifier_node)
                                if (param1%name == "a") then
                                    print '(a)', "PASS: First parameter name is 'a'"
                                else
                                    print '(a)', "FAIL: Wrong first parameter name"
                                    test_simple_subroutine_definition = .false.
                                end if
                            class default
                               print '(a)', "FAIL: First parameter is not an identifier"
                                test_simple_subroutine_definition = .false.
                            end select
                        else
                            print '(a)', "FAIL: First parameter node not allocated"
                            test_simple_subroutine_definition = .false.
                        end if

                        ! Check second parameter
                        if (allocated(stmt%params(2)%node)) then
                            select type (param2 => stmt%params(2)%node)
                            type is (identifier_node)
                                if (param2%name == "b") then
                                    print '(a)', "PASS: Second parameter name is 'b'"
                                else
                                    print '(a)', "FAIL: Wrong second parameter name"
                                    test_simple_subroutine_definition = .false.
                                end if
                            class default
                              print '(a)', "FAIL: Second parameter is not an identifier"
                                test_simple_subroutine_definition = .false.
                            end select
                        else
                            print '(a)', "FAIL: Second parameter node not allocated"
                            test_simple_subroutine_definition = .false.
                        end if
                    else
                  print '(a,i0)', "FAIL: Expected 2 parameters, got ", size(stmt%params)
                        test_simple_subroutine_definition = .false.
                    end if
                end if

                ! Check body
                if (.not. allocated(stmt%body)) then
       print '(a)', "INFO: No body in subroutine definition (expected for minimal test)"
                else
            print '(a,i0)', "INFO: Subroutine body has ", size(stmt%body), " statements"
                end if

            class default
                print '(a)', "FAIL: Subroutine definition parsed as wrong node type"
                test_simple_subroutine_definition = .false.
            end select
        end if

    end function test_simple_subroutine_definition

    logical function test_subroutine_with_parameters()
        ! TDD Test 2: Parse subroutine with multiple parameters
        ! subroutine process(input, output, flag)
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_subroutine_with_parameters = .true.

        print '(a)', "Testing subroutine with multiple parameters..."

        ! Test: subroutine process(input, output, flag) end subroutine
    call tokenize_core("subroutine process(input, output, flag) end subroutine", tokens)

        ! Add EOF token
        block
            type(token_t), allocatable :: tokens_with_eof(:)
            integer :: n

            n = size(tokens)
            allocate (tokens_with_eof(n + 1))
            tokens_with_eof(1:n) = tokens
            tokens_with_eof(n + 1)%kind = TK_EOF
            tokens_with_eof(n + 1)%text = ""
            tokens_with_eof(n + 1)%line = 1
            tokens_with_eof(n + 1)%column = 1

            stmt = parse_statement(tokens_with_eof)
        end block

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: No AST node returned for subroutine with parameters"
            test_subroutine_with_parameters = .false.
        else
            select type (stmt)
            type is (subroutine_def_node)
           print '(a)', "PASS: Subroutine with parameters parsed as subroutine_def_node"

                ! Check subroutine name
                if (allocated(stmt%name)) then
                    if (stmt%name == "process") then
                        print '(a)', "PASS: Subroutine name is 'process'"
                    else
                        print '(a)', "FAIL: Wrong subroutine name"
                        test_subroutine_with_parameters = .false.
                    end if
                end if

                ! Check parameter count
                if (.not. allocated(stmt%params)) then
                    print '(a)', "FAIL: No parameters in subroutine with parameters"
                    test_subroutine_with_parameters = .false.
                else
                    if (size(stmt%params) == 3) then
                        print '(a)', "PASS: Subroutine has 3 parameters"
                    else
                  print '(a,i0)', "FAIL: Expected 3 parameters, got ", size(stmt%params)
                        test_subroutine_with_parameters = .false.
                    end if
                end if

            class default
               print '(a)', "FAIL: Subroutine with parameters parsed as wrong node type"
                test_subroutine_with_parameters = .false.
            end select
        end if

    end function test_subroutine_with_parameters

    logical function test_subroutine_no_parameters()
        ! TDD Test 3: Parse subroutine with no parameters
        ! subroutine initialize()
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_subroutine_no_parameters = .true.

        print '(a)', "Testing subroutine with no parameters..."

        ! Test: subroutine initialize() end subroutine
        call tokenize_core("subroutine initialize() end subroutine", tokens)

        ! Add EOF token
        block
            type(token_t), allocatable :: tokens_with_eof(:)
            integer :: n

            n = size(tokens)
            allocate (tokens_with_eof(n + 1))
            tokens_with_eof(1:n) = tokens
            tokens_with_eof(n + 1)%kind = TK_EOF
            tokens_with_eof(n + 1)%text = ""
            tokens_with_eof(n + 1)%line = 1
            tokens_with_eof(n + 1)%column = 1

            stmt = parse_statement(tokens_with_eof)
        end block

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: No AST node returned for subroutine with no parameters"
            test_subroutine_no_parameters = .false.
        else
            select type (stmt)
            type is (subroutine_def_node)
        print '(a)', "PASS: Subroutine with no parameters parsed as subroutine_def_node"

                ! Check subroutine name
                if (allocated(stmt%name)) then
                    if (stmt%name == "initialize") then
                        print '(a)', "PASS: Subroutine name is 'initialize'"
                    else
                        print '(a)', "FAIL: Wrong subroutine name"
                        test_subroutine_no_parameters = .false.
                    end if
                end if

                ! Check parameters (should be empty or not allocated)
                if (.not. allocated(stmt%params)) then
                    print '(a)', "PASS: No parameters (expected for initialize())"
                else
                    if (size(stmt%params) == 0) then
                        print '(a)', "PASS: Empty parameter list"
                    else
                  print '(a,i0)', "FAIL: Expected 0 parameters, got ", size(stmt%params)
                        test_subroutine_no_parameters = .false.
                    end if
                end if

            class default
            print '(a)', "FAIL: Subroutine with no parameters parsed as wrong node type"
                test_subroutine_no_parameters = .false.
            end select
        end if

    end function test_subroutine_no_parameters

    logical function test_subroutine_with_intent()
        ! TDD Test 4: Parse subroutine (basic test, intent handling may be future work)
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_subroutine_with_intent = .true.

        print '(a)', "Testing subroutine with intent (basic parsing)..."

        ! Test: subroutine compute(x, y) end subroutine
        call tokenize_core("subroutine compute(x, y) end subroutine", tokens)

        ! Add EOF token
        block
            type(token_t), allocatable :: tokens_with_eof(:)
            integer :: n

            n = size(tokens)
            allocate (tokens_with_eof(n + 1))
            tokens_with_eof(1:n) = tokens
            tokens_with_eof(n + 1)%kind = TK_EOF
            tokens_with_eof(n + 1)%text = ""
            tokens_with_eof(n + 1)%line = 1
            tokens_with_eof(n + 1)%column = 1

            stmt = parse_statement(tokens_with_eof)
        end block

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: No AST node returned for subroutine with intent"
            test_subroutine_with_intent = .false.
        else
            select type (stmt)
            type is (subroutine_def_node)
               print '(a)', "PASS: Subroutine with intent parsed as subroutine_def_node"

                ! Check subroutine name
                if (allocated(stmt%name)) then
                    if (stmt%name == "compute") then
                        print '(a)', "PASS: Subroutine name is 'compute'"
                    else
                        print '(a)', "FAIL: Wrong subroutine name"
                        test_subroutine_with_intent = .false.
                    end if
                end if

            class default
                print '(a)', "FAIL: Subroutine with intent parsed as wrong node type"
                test_subroutine_with_intent = .false.
            end select
        end if

    end function test_subroutine_with_intent

end program test_frontend_parser_subroutine_definitions
