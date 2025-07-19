program test_frontend_parser_generic_interfaces
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for generic interfaces
    if (.not. test_simple_interface()) all_passed = .false.
    if (.not. test_named_interface()) all_passed = .false.
    if (.not. test_operator_interface()) all_passed = .false.
    if (.not. test_assignment_interface()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All generic interface parser tests passed"
        stop 0
    else
        print '(a)', "Some generic interface parser tests failed"
        stop 1
    end if

contains

    logical function test_simple_interface()
        ! TDD Test 1: Parse simple interface
        ! interface
        !     function func(x)
        !     end function
        ! end interface
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_simple_interface = .true.

        print '(a)', "Testing simple interface..."

        ! Test: interface end interface
        call tokenize_core("interface end interface", tokens)

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
            print '(a)', "FAIL: No AST node returned for simple interface"
            test_simple_interface = .false.
        else
            ! Check what type of node we get
            select type (stmt)
            type is (interface_block_node)
                print '(a)', "PASS: Simple interface parsed as interface_block_node"

                ! Check interface kind
                if (allocated(stmt%kind)) then
                    if (stmt%kind == "interface") then
                        print '(a)', "PASS: Interface kind is 'interface'"
                    else
                        print '(a)', "FAIL: Wrong interface kind"
                        test_simple_interface = .false.
                    end if
                else
                    print '(a)', "FAIL: No interface kind"
                    test_simple_interface = .false.
                end if

                ! Check interface name (should be empty for simple interface)
                if (allocated(stmt%name)) then
                    if (len(stmt%name) == 0) then
           print '(a)', "PASS: Interface has empty name (expected for simple interface)"
                    else
                        print '(a)', "INFO: Interface name is not empty"
                    end if
                else
                  print '(a)', "PASS: No interface name (expected for simple interface)"
                end if

                ! Check procedures
                if (allocated(stmt%procedures)) then
          print '(a,i0)', "INFO: Interface has ", size(stmt%procedures), " procedure(s)"
                else
                    print '(a)', "INFO: No procedures array"
                end if

            class default
                print '(a)', "FAIL: Simple interface parsed as wrong node type"
                test_simple_interface = .false.
            end select
        end if

    end function test_simple_interface

    logical function test_named_interface()
        ! TDD Test 2: Parse named interface
        ! interface solve
        !     function solve_linear(a, b)
        !     end function
        !     function solve_quadratic(a, b, c)
        !     end function
        ! end interface
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_named_interface = .true.

        print '(a)', "Testing named interface..."

        ! Test: interface solve end interface
        call tokenize_core("interface solve end interface", tokens)

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
            print '(a)', "FAIL: No AST node returned for named interface"
            test_named_interface = .false.
        else
            select type (stmt)
            type is (interface_block_node)
                print '(a)', "PASS: Named interface parsed as interface_block_node"

                ! Check interface kind
                if (allocated(stmt%kind)) then
                    if (stmt%kind == "generic") then
                        print '(a)', "PASS: Interface kind is 'generic'"
                    else
                        print '(a)', "INFO: Interface kind is not 'generic'"
                    end if
                else
                    print '(a)', "FAIL: No interface kind"
                    test_named_interface = .false.
                end if

                ! Check interface name
                if (allocated(stmt%name)) then
                    if (stmt%name == "solve") then
                        print '(a)', "PASS: Interface name is 'solve'"
                    else
                        print '(a)', "FAIL: Wrong interface name"
                        test_named_interface = .false.
                    end if
                else
                    print '(a)', "FAIL: No interface name"
                    test_named_interface = .false.
                end if

            class default
                print '(a)', "FAIL: Named interface parsed as wrong node type"
                test_named_interface = .false.
            end select
        end if

    end function test_named_interface

    logical function test_operator_interface()
        ! TDD Test 3: Parse operator interface
        ! interface operator(+)
        !     function add_complex(a, b)
        !     end function
        ! end interface
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_operator_interface = .true.

        print '(a)', "Testing operator interface..."

        ! Test: interface operator(+) end interface
        call tokenize_core("interface operator(+) end interface", tokens)

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
            print '(a)', "FAIL: No AST node returned for operator interface"
            test_operator_interface = .false.
        else
            select type (stmt)
            type is (interface_block_node)
                print '(a)', "PASS: Operator interface parsed as interface_block_node"

                ! Check interface kind
                if (allocated(stmt%kind)) then
                    if (stmt%kind == "operator") then
                        print '(a)', "PASS: Interface kind is 'operator'"
                    else
                        print '(a)', "INFO: Interface kind is not 'operator'"
                    end if
                else
                    print '(a)', "FAIL: No interface kind"
                    test_operator_interface = .false.
                end if

                ! Check operator symbol
                if (allocated(stmt%operator)) then
                    if (stmt%operator == "+") then
                        print '(a)', "PASS: Operator symbol is '+'"
                    else
                        print '(a)', "FAIL: Wrong operator symbol"
                        test_operator_interface = .false.
                    end if
                else
                    print '(a)', "FAIL: No operator symbol"
                    test_operator_interface = .false.
                end if

            class default
                print '(a)', "FAIL: Operator interface parsed as wrong node type"
                test_operator_interface = .false.
            end select
        end if

    end function test_operator_interface

    logical function test_assignment_interface()
        ! TDD Test 4: Parse assignment interface
        ! interface assignment(=)
        !     subroutine assign_complex(a, b)
        !     end subroutine
        ! end interface
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_assignment_interface = .true.

        print '(a)', "Testing assignment interface..."

        ! Test: interface assignment(=) end interface
        call tokenize_core("interface assignment(=) end interface", tokens)

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
            print '(a)', "FAIL: No AST node returned for assignment interface"
            test_assignment_interface = .false.
        else
            select type (stmt)
            type is (interface_block_node)
                print '(a)', "PASS: Assignment interface parsed as interface_block_node"

                ! Check interface kind
                if (allocated(stmt%kind)) then
                    if (stmt%kind == "assignment") then
                        print '(a)', "PASS: Interface kind is 'assignment'"
                    else
                        print '(a)', "INFO: Interface kind is not 'assignment'"
                    end if
                else
                    print '(a)', "FAIL: No interface kind"
                    test_assignment_interface = .false.
                end if

                ! Check operator symbol (should be '=' for assignment)
                if (allocated(stmt%operator)) then
                    if (stmt%operator == "=") then
                        print '(a)', "PASS: Assignment operator is '='"
                    else
                        print '(a)', "FAIL: Wrong assignment operator"
                        test_assignment_interface = .false.
                    end if
                else
                    print '(a)', "FAIL: No assignment operator"
                    test_assignment_interface = .false.
                end if

            class default
                print '(a)', "FAIL: Assignment interface parsed as wrong node type"
                test_assignment_interface = .false.
            end select
        end if

    end function test_assignment_interface

end program test_frontend_parser_generic_interfaces
