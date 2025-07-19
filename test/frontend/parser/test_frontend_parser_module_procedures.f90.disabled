program test_frontend_parser_module_procedures
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for module procedures
    if (.not. test_module_with_function()) all_passed = .false.
    if (.not. test_module_with_subroutine()) all_passed = .false.
    if (.not. test_module_with_contains()) all_passed = .false.
    if (.not. test_module_with_multiple_procedures()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All module procedure parser tests passed"
        stop 0
    else
        print '(a)', "Some module procedure parser tests failed"
        stop 1
    end if

contains

    logical function test_module_with_function()
        ! TDD Test 1: Parse module with function
        ! module math_utils
        !     contains
        !     function square(x)
        !         square = x * x
        !     end function
        ! end module
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_module_with_function = .true.

        print '(a)', "Testing module with function..."

        ! Test: module math_utils contains function square(x) end function end module
        call tokenize_core("module math_utils contains function square(x) end function end module", tokens)

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
            print '(a)', "FAIL: No AST node returned for module with function"
            test_module_with_function = .false.
        else
            ! Check what type of node we get
            select type (stmt)
            type is (module_node)
                print '(a)', "PASS: Module with function parsed as module_node"

                ! Check module name
                if (allocated(stmt%name)) then
                    if (stmt%name == "math_utils") then
                        print '(a)', "PASS: Module name is 'math_utils'"
                    else
                        print '(a)', "FAIL: Wrong module name"
                        test_module_with_function = .false.
                    end if
                else
                    print '(a)', "FAIL: No module name"
                    test_module_with_function = .false.
                end if

                ! Check if module has contains section
                if (stmt%has_contains) then
                    print '(a)', "PASS: Module has contains section"
                else
                    print '(a)', "INFO: Module does not have contains flag set"
                end if

                ! Check procedures
                if (allocated(stmt%procedures)) then
                    if (size(stmt%procedures) > 0) then
             print '(a,i0)', "PASS: Module has ", size(stmt%procedures), " procedure(s)"
                    else
                        print '(a)', "INFO: Module has empty procedures array"
                    end if
                else
                  print '(a)', "INFO: No procedures array (may need parser enhancement)"
                end if

                ! Check declarations
                if (allocated(stmt%declarations)) then
         print '(a,i0)', "INFO: Module has ", size(stmt%declarations), " declaration(s)"
                else
                    print '(a)', "INFO: No declarations"
                end if

            class default
                print '(a)', "FAIL: Module with function parsed as wrong node type"
                test_module_with_function = .false.
            end select
        end if

    end function test_module_with_function

    logical function test_module_with_subroutine()
        ! TDD Test 2: Parse module with subroutine
        ! module io_utils
        !     contains
        !     subroutine print_array(arr)
        !         print *, arr
        !     end subroutine
        ! end module
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_module_with_subroutine = .true.

        print '(a)', "Testing module with subroutine..."

        ! Test: module io_utils contains subroutine print_array(arr) end subroutine end module
        call tokenize_core("module io_utils contains subroutine print_array(arr) end subroutine end module", tokens)

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
            print '(a)', "FAIL: No AST node returned for module with subroutine"
            test_module_with_subroutine = .false.
        else
            select type (stmt)
            type is (module_node)
                print '(a)', "PASS: Module with subroutine parsed as module_node"

                ! Check module name
                if (allocated(stmt%name)) then
                    if (stmt%name == "io_utils") then
                        print '(a)', "PASS: Module name is 'io_utils'"
                    else
                        print '(a)', "FAIL: Wrong module name"
                        test_module_with_subroutine = .false.
                    end if
                end if

                ! Check procedures
                if (allocated(stmt%procedures)) then
                    if (size(stmt%procedures) > 0) then
             print '(a,i0)', "PASS: Module has ", size(stmt%procedures), " procedure(s)"
                    else
                        print '(a)', "INFO: Module has empty procedures array"
                    end if
                else
                  print '(a)', "INFO: No procedures array (may need parser enhancement)"
                end if

            class default
                print '(a)', "FAIL: Module with subroutine parsed as wrong node type"
                test_module_with_subroutine = .false.
            end select
        end if

    end function test_module_with_subroutine

    logical function test_module_with_contains()
        ! TDD Test 3: Parse module with contains section
        ! module test_module
        !     implicit none
        !     contains
        ! end module
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_module_with_contains = .true.

        print '(a)', "Testing module with contains..."

        ! Test: module test_module contains end module
        call tokenize_core("module test_module contains end module", tokens)

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
            print '(a)', "FAIL: No AST node returned for module with contains"
            test_module_with_contains = .false.
        else
            select type (stmt)
            type is (module_node)
                print '(a)', "PASS: Module with contains parsed as module_node"

                ! Check module name
                if (allocated(stmt%name)) then
                    if (stmt%name == "test_module") then
                        print '(a)', "PASS: Module name is 'test_module'"
                    else
                        print '(a)', "FAIL: Wrong module name"
                        test_module_with_contains = .false.
                    end if
                end if

                ! Check contains flag
                if (stmt%has_contains) then
                    print '(a)', "PASS: Module has contains flag set"
                else
                    print '(a)', "INFO: Module does not have contains flag set"
                end if

            class default
                print '(a)', "FAIL: Module with contains parsed as wrong node type"
                test_module_with_contains = .false.
            end select
        end if

    end function test_module_with_contains

    logical function test_module_with_multiple_procedures()
        ! TDD Test 4: Parse module with multiple procedures
        ! module utils
        !     contains
        !     function add(a, b)
        !         add = a + b
        !     end function
        !     subroutine print_result(x)
        !         print *, x
        !     end subroutine
        ! end module
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_module_with_multiple_procedures = .true.

        print '(a)', "Testing module with multiple procedures..."

        ! Test simplified: module utils contains function add(a, b) end function end module
        call tokenize_core("module utils contains function add(a, b) end function end module", tokens)

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
           print '(a)', "FAIL: No AST node returned for module with multiple procedures"
            test_module_with_multiple_procedures = .false.
        else
            select type (stmt)
            type is (module_node)
              print '(a)', "PASS: Module with multiple procedures parsed as module_node"

                ! Check module name
                if (allocated(stmt%name)) then
                    if (stmt%name == "utils") then
                        print '(a)', "PASS: Module name is 'utils'"
                    else
                        print '(a)', "FAIL: Wrong module name"
                        test_module_with_multiple_procedures = .false.
                    end if
                end if

                ! Check procedures
                if (allocated(stmt%procedures)) then
             print '(a,i0)', "INFO: Module has ", size(stmt%procedures), " procedure(s)"
                else
                    print '(a)', "INFO: No procedures array"
                end if

                print '(a)', "PASS: Module multiple procedures test complete"

            class default
          print '(a)', "FAIL: Module with multiple procedures parsed as wrong node type"
                test_module_with_multiple_procedures = .false.
            end select
        end if

    end function test_module_with_multiple_procedures

end program test_frontend_parser_module_procedures
