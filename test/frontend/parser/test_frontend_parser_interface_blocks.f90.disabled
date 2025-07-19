program test_frontend_parser_interface_blocks
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for interface blocks
    if (.not. test_simple_interface_block()) all_passed = .false.
    if (.not. test_generic_interface()) all_passed = .false.
    if (.not. test_operator_interface()) all_passed = .false.
    if (.not. test_assignment_interface()) all_passed = .false.
    if (.not. test_interface_with_procedures()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All interface blocks parser tests passed"
        stop 0
    else
        print '(a)', "Some interface blocks parser tests failed"
        stop 1
    end if

contains

    logical function test_simple_interface_block()
        ! TDD Test 1: Parse simple interface block
        ! interface
        !     function func(x)
        !         real :: func
        !         real, intent(in) :: x
        !     end function func
        ! end interface
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_simple_interface_block = .true.

        print '(a)', "Testing simple interface block parsing..."

        ! Test basic interface with end interface
        call tokenize_core("interface end interface", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we get an interface_block_node
        if (.not. allocated(ast_result)) then
            print '(a)', "FAIL: No AST node returned"
            test_simple_interface_block = .false.
        else
            select type (ast_result)
            type is (interface_block_node)
                if (ast_result%kind == "interface") then
                    print '(a)', "PASS: Basic interface block parsed correctly"
                else
                    print '(a)', "FAIL: Wrong interface kind"
                    test_simple_interface_block = .false.
                end if
            class default
                print '(a)', "FAIL: Expected interface_block_node"
                test_simple_interface_block = .false.
            end select
        end if

    end function test_simple_interface_block

    logical function test_generic_interface()
        ! TDD Test 2: Parse generic interface
        ! interface generic_name
        !     procedure :: specific_proc1, specific_proc2
        ! end interface
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_generic_interface = .true.

        print '(a)', "Testing generic interface parsing..."

        ! Test named interface
        call tokenize_core("interface generic_name end interface", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we get a generic interface_block_node
        if (.not. allocated(ast_result)) then
            print '(a)', "FAIL: No AST node returned"
            test_generic_interface = .false.
        else
            select type (ast_result)
            type is (interface_block_node)
          if (ast_result%kind == "generic" .and. ast_result%name == "generic_name") then
                    print '(a)', "PASS: Generic interface parsed correctly"
                else
                    print '(a)', "FAIL: Wrong interface kind or name"
                    test_generic_interface = .false.
                end if
            class default
                print '(a)', "FAIL: Expected interface_block_node"
                test_generic_interface = .false.
            end select
        end if

    end function test_generic_interface

    logical function test_operator_interface()
        ! TDD Test 3: Parse operator interface
        ! interface operator(+)
        !     procedure :: custom_add
        ! end interface
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_operator_interface = .true.

        print '(a)', "Testing operator interface parsing..."

        ! Test operator interface
        call tokenize_core("interface operator(+) end interface", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we get an operator interface_block_node
        if (.not. allocated(ast_result)) then
            print '(a)', "FAIL: No AST node returned"
            test_operator_interface = .false.
        else
            select type (ast_result)
            type is (interface_block_node)
                if (ast_result%kind == "operator" .and. ast_result%operator == "+") then
                    print '(a)', "PASS: Operator interface parsed correctly"
                else
     print '(a,a)', "FAIL: Wrong interface kind or operator. Got kind=", ast_result%kind
                    if (allocated(ast_result%operator)) then
                        print '(a,a)', "       operator=", ast_result%operator
                    else
                        print '(a)', "       operator=<not allocated>"
                    end if
                    test_operator_interface = .false.
                end if
            class default
                print '(a)', "FAIL: Expected interface_block_node"
                test_operator_interface = .false.
            end select
        end if

    end function test_operator_interface

    logical function test_assignment_interface()
        ! TDD Test 4: Parse assignment interface
        ! interface assignment(=)
        !     procedure :: custom_assign
        ! end interface
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_assignment_interface = .true.

        print '(a)', "Testing assignment interface parsing..."

        ! Test assignment interface
        call tokenize_core("interface assignment(=) end interface", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we get an assignment interface_block_node
        if (.not. allocated(ast_result)) then
            print '(a)', "FAIL: No AST node returned"
            test_assignment_interface = .false.
        else
            select type (ast_result)
            type is (interface_block_node)
                if (ast_result%kind == "assignment" .and. allocated(ast_result%operator) .and. ast_result%operator == "=") then
                    print '(a)', "PASS: Assignment interface parsed correctly"
                else
     print '(a,a)', "FAIL: Wrong interface kind or operator. Got kind=", ast_result%kind
                    if (allocated(ast_result%operator)) then
                        print '(a,a)', "       operator=", ast_result%operator
                    else
                        print '(a)', "       operator=<not allocated>"
                    end if
                    test_assignment_interface = .false.
                end if
            class default
                print '(a)', "FAIL: Expected interface_block_node"
                test_assignment_interface = .false.
            end select
        end if

    end function test_assignment_interface

    logical function test_interface_with_procedures()
        ! TDD Test 5: Parse interface block with procedure declarations
        ! interface
        !     function func(x)
        !         real :: func
        !         real, intent(in) :: x
        !     end function func
        ! end interface
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_interface_with_procedures = .true.

        print '(a)', "Testing interface block with procedures..."

        ! Test interface with simple procedure signature
        call tokenize_core("interface function func(x) real :: func real, intent(in) :: x end function func end interface", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we get an interface_block_node
        if (.not. allocated(ast_result)) then
            print '(a)', "FAIL: No AST node returned"
            test_interface_with_procedures = .false.
        else
            select type (ast_result)
            type is (interface_block_node)
                if (ast_result%kind == "interface") then
                    ! Interface structure parsed successfully
                    ! Procedure body parsing is a future enhancement
                    print '(a)', "PASS: Interface block with procedures parsed (body parsing not yet implemented)"
                else
                    print '(a)', "FAIL: Wrong interface kind"
                    test_interface_with_procedures = .false.
                end if
            class default
                print '(a)', "FAIL: Expected interface_block_node"
                test_interface_with_procedures = .false.
            end select
        end if

    end function test_interface_with_procedures

end program test_frontend_parser_interface_blocks
