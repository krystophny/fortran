program test_frontend_parser_operator_overloading
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for operator overloading
    if (.not. test_binary_operator_overloading()) all_passed = .false.
    if (.not. test_unary_operator_overloading()) all_passed = .false.
    if (.not. test_multiple_operator_overloading()) all_passed = .false.
    if (.not. test_assignment_operator_overloading()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All operator overloading parser tests passed"
        stop 0
    else
        print '(a)', "Some operator overloading parser tests failed"
        stop 1
    end if

contains

    logical function test_binary_operator_overloading()
        ! TDD Test 1: Parse binary operator overloading
        ! interface operator(+)
        !     function add_complex(a, b)
        !         type(complex), intent(in) :: a, b
        !         type(complex) :: add_complex
        !         add_complex = complex(a%real + b%real, a%imag + b%imag)
        !     end function
        ! end interface
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_binary_operator_overloading = .true.

        print '(a)', "Testing binary operator overloading..."

        ! Test: interface operator(+) function add_complex(a, b) end function end interface
        call tokenize_core("interface operator(+) function add_complex(a, b) end function end interface", tokens)

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
            print '(a)', "FAIL: No AST node returned for binary operator overloading"
            test_binary_operator_overloading = .false.
        else
            select type (stmt)
            type is (interface_block_node)
         print '(a)', "PASS: Binary operator overloading parsed as interface_block_node"

                ! Check interface kind
                if (allocated(stmt%kind)) then
                    if (stmt%kind == "operator") then
                        print '(a)', "PASS: Interface kind is 'operator'"
                    else
                        print '(a)', "FAIL: Wrong interface kind"
                        test_binary_operator_overloading = .false.
                    end if
                else
                    print '(a)', "FAIL: No interface kind"
                    test_binary_operator_overloading = .false.
                end if

                ! Check operator symbol
                if (allocated(stmt%operator)) then
                    if (stmt%operator == "+") then
                        print '(a)', "PASS: Operator symbol is '+'"
                    else
                        print '(a)', "FAIL: Wrong operator symbol"
                        test_binary_operator_overloading = .false.
                    end if
                else
                    print '(a)', "FAIL: No operator symbol"
                    test_binary_operator_overloading = .false.
                end if

                ! Check procedures
                if (allocated(stmt%procedures)) then
                    if (size(stmt%procedures) > 0) then
          print '(a,i0)', "PASS: Interface has ", size(stmt%procedures), " procedure(s)"
                    else
                        print '(a)', "INFO: Interface has empty procedures array"
                    end if
                else
                    print '(a)', "INFO: No procedures array"
                end if

            class default
              print '(a)', "FAIL: Binary operator overloading parsed as wrong node type"
                test_binary_operator_overloading = .false.
            end select
        end if

    end function test_binary_operator_overloading

    logical function test_unary_operator_overloading()
        ! TDD Test 2: Parse unary operator overloading
        ! interface operator(-)
        !     function negate_complex(a)
        !         type(complex), intent(in) :: a
        !         type(complex) :: negate_complex
        !         negate_complex = complex(-a%real, -a%imag)
        !     end function
        ! end interface
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_unary_operator_overloading = .true.

        print '(a)', "Testing unary operator overloading..."

        ! Test: interface operator(-) function negate_complex(a) end function end interface
        call tokenize_core("interface operator(-) function negate_complex(a) end function end interface", tokens)

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
            print '(a)', "FAIL: No AST node returned for unary operator overloading"
            test_unary_operator_overloading = .false.
        else
            select type (stmt)
            type is (interface_block_node)
          print '(a)', "PASS: Unary operator overloading parsed as interface_block_node"

                ! Check interface kind
                if (allocated(stmt%kind)) then
                    if (stmt%kind == "operator") then
                        print '(a)', "PASS: Interface kind is 'operator'"
                    else
                        print '(a)', "FAIL: Wrong interface kind"
                        test_unary_operator_overloading = .false.
                    end if
                else
                    print '(a)', "FAIL: No interface kind"
                    test_unary_operator_overloading = .false.
                end if

                ! Check operator symbol
                if (allocated(stmt%operator)) then
                    if (stmt%operator == "-") then
                        print '(a)', "PASS: Operator symbol is '-'"
                    else
                        print '(a)', "FAIL: Wrong operator symbol"
                        test_unary_operator_overloading = .false.
                    end if
                else
                    print '(a)', "FAIL: No operator symbol"
                    test_unary_operator_overloading = .false.
                end if

            class default
               print '(a)', "FAIL: Unary operator overloading parsed as wrong node type"
                test_unary_operator_overloading = .false.
            end select
        end if

    end function test_unary_operator_overloading

    logical function test_multiple_operator_overloading()
        ! TDD Test 3: Parse multiple operator overloading
        ! interface operator(*)
        !     function multiply_complex(a, b)
        !         type(complex), intent(in) :: a, b
        !         type(complex) :: multiply_complex
        !         multiply_complex = complex(a%real * b%real - a%imag * b%imag, a%real * b%imag + a%imag * b%real)
        !     end function
        ! end interface
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_multiple_operator_overloading = .true.

        print '(a)', "Testing multiple operator overloading..."

        ! Test: interface operator(*) function multiply_complex(a, b) end function end interface
        call tokenize_core("interface operator(*) function multiply_complex(a, b) end function end interface", tokens)

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
            print '(a)', "FAIL: No AST node returned for multiple operator overloading"
            test_multiple_operator_overloading = .false.
        else
            select type (stmt)
            type is (interface_block_node)
       print '(a)', "PASS: Multiple operator overloading parsed as interface_block_node"

                ! Check interface kind
                if (allocated(stmt%kind)) then
                    if (stmt%kind == "operator") then
                        print '(a)', "PASS: Interface kind is 'operator'"
                    else
                        print '(a)', "FAIL: Wrong interface kind"
                        test_multiple_operator_overloading = .false.
                    end if
                else
                    print '(a)', "FAIL: No interface kind"
                    test_multiple_operator_overloading = .false.
                end if

                ! Check operator symbol
                if (allocated(stmt%operator)) then
                    if (stmt%operator == "*") then
                        print '(a)', "PASS: Operator symbol is '*'"
                    else
                        print '(a)', "FAIL: Wrong operator symbol"
                        test_multiple_operator_overloading = .false.
                    end if
                else
                    print '(a)', "FAIL: No operator symbol"
                    test_multiple_operator_overloading = .false.
                end if

            class default
            print '(a)', "FAIL: Multiple operator overloading parsed as wrong node type"
                test_multiple_operator_overloading = .false.
            end select
        end if

    end function test_multiple_operator_overloading

    logical function test_assignment_operator_overloading()
        ! TDD Test 4: Parse assignment operator overloading
        ! interface assignment(=)
        !     subroutine assign_complex(a, b)
        !         type(complex), intent(out) :: a
        !         type(complex), intent(in) :: b
        !         a = b
        !     end subroutine
        ! end interface
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_assignment_operator_overloading = .true.

        print '(a)', "Testing assignment operator overloading..."

        ! Test: interface assignment(=) subroutine assign_complex(a, b) end subroutine end interface
        call tokenize_core("interface assignment(=) subroutine assign_complex(a, b) end subroutine end interface", tokens)

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
           print '(a)', "FAIL: No AST node returned for assignment operator overloading"
            test_assignment_operator_overloading = .false.
        else
            select type (stmt)
            type is (interface_block_node)
     print '(a)', "PASS: Assignment operator overloading parsed as interface_block_node"

                ! Check interface kind
                if (allocated(stmt%kind)) then
                    if (stmt%kind == "assignment") then
                        print '(a)', "PASS: Interface kind is 'assignment'"
                    else
                        print '(a)', "FAIL: Wrong interface kind"
                        test_assignment_operator_overloading = .false.
                    end if
                else
                    print '(a)', "FAIL: No interface kind"
                    test_assignment_operator_overloading = .false.
                end if

                ! Check operator symbol
                if (allocated(stmt%operator)) then
                    if (stmt%operator == "=") then
                        print '(a)', "PASS: Assignment operator is '='"
                    else
                        print '(a)', "FAIL: Wrong assignment operator"
                        test_assignment_operator_overloading = .false.
                    end if
                else
                    print '(a)', "FAIL: No assignment operator"
                    test_assignment_operator_overloading = .false.
                end if

            class default
          print '(a)', "FAIL: Assignment operator overloading parsed as wrong node type"
                test_assignment_operator_overloading = .false.
            end select
        end if

    end function test_assignment_operator_overloading

end program test_frontend_parser_operator_overloading
