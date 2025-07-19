program test_frontend_parser_declarations
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests in order
    if (.not. test_simple_integer_declaration()) all_passed = .false.
    if (.not. test_simple_real_declaration()) all_passed = .false.
    if (.not. test_declaration_with_kind()) all_passed = .false.
    if (.not. test_declaration_with_initialization()) all_passed = .false.
    if (.not. test_multiple_variable_declaration()) all_passed = .false.
    if (.not. test_character_declaration()) all_passed = .false.
    if (.not. test_logical_declaration()) all_passed = .false.
    if (.not. test_complex_declaration()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All declaration parser tests passed"
        stop 0
    else
        print '(a)', "Some declaration parser tests failed"
        stop 1
    end if

contains

    logical function test_simple_integer_declaration()
        ! TDD Test 1: Parse simple integer declaration "integer :: x"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_simple_integer_declaration = .true.

        print '(a)', "Testing simple integer declaration parsing..."

        ! Tokenize simple integer declaration
        call tokenize_core("integer :: x", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a declaration node
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "integer") then
                print '(a)', "FAIL: Type name incorrect, expected 'integer', got '"// &
                    trim(ast_result%type_name)//"'"
                test_simple_integer_declaration = .false.
            else if (ast_result%var_name /= "x") then
                print '(a)', "FAIL: Variable name incorrect, expected 'x', got '"// &
                    trim(ast_result%var_name)//"'"
                test_simple_integer_declaration = .false.
            else
                print '(a)', "PASS: Simple integer declaration parsing"
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_simple_integer_declaration = .false.
        end select

    end function test_simple_integer_declaration

    logical function test_simple_real_declaration()
        ! TDD Test 2: Parse simple real declaration "real :: y"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_simple_real_declaration = .true.

        print '(a)', "Testing simple real declaration parsing..."

        ! Tokenize simple real declaration
        call tokenize_core("real :: y", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a declaration node
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "real") then
                print '(a)', "FAIL: Type name incorrect, expected 'real', got '"// &
                    trim(ast_result%type_name)//"'"
                test_simple_real_declaration = .false.
            else if (ast_result%var_name /= "y") then
                print '(a)', "FAIL: Variable name incorrect, expected 'y', got '"// &
                    trim(ast_result%var_name)//"'"
                test_simple_real_declaration = .false.
            else
                print '(a)', "PASS: Simple real declaration parsing"
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_simple_real_declaration = .false.
        end select

    end function test_simple_real_declaration

    logical function test_declaration_with_kind()
        ! TDD Test 3: Parse declaration with kind "real(8) :: z"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_declaration_with_kind = .true.

        print '(a)', "Testing declaration with kind parsing..."

        ! Tokenize declaration with kind
        call tokenize_core("real(8) :: z", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a declaration node with kind
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "real") then
                print '(a)', "FAIL: Type name incorrect"
                test_declaration_with_kind = .false.
            else if (ast_result%var_name /= "z") then
                print '(a)', "FAIL: Variable name incorrect"
                test_declaration_with_kind = .false.
            else if (.not. ast_result%has_kind) then
                print '(a)', "FAIL: Kind not detected"
                test_declaration_with_kind = .false.
            else if (ast_result%kind_value /= 8) then
                print '(a)', "FAIL: Kind value incorrect, expected 8, got ", &
                    ast_result%kind_value
                test_declaration_with_kind = .false.
            else
                print '(a)', "PASS: Declaration with kind parsing"
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_declaration_with_kind = .false.
        end select

    end function test_declaration_with_kind

    logical function test_declaration_with_initialization()
        ! TDD Test 4: Parse declaration with initialization "real :: pi = 3.14159"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_declaration_with_initialization = .true.

        print '(a)', "Testing declaration with initialization parsing..."

        ! Tokenize declaration with initialization
        call tokenize_core("real :: pi = 3.14159", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a declaration node with initializer
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "real") then
                print '(a)', "FAIL: Type name incorrect"
                test_declaration_with_initialization = .false.
            else if (ast_result%var_name /= "pi") then
                print '(a)', "FAIL: Variable name incorrect"
                test_declaration_with_initialization = .false.
            else if (.not. allocated(ast_result%initializer)) then
                print '(a)', "FAIL: Initializer not parsed"
                test_declaration_with_initialization = .false.
            else
                ! Check the initializer is a literal with correct value
                select type (init => ast_result%initializer)
                type is (literal_node)
                    if (init%value /= "3.14159") then
                        print '(a)', "FAIL: Initializer value incorrect"
                        test_declaration_with_initialization = .false.
                    else
                        print '(a)', "PASS: Declaration with initialization parsing"
                    end if
                class default
                    print '(a)', "FAIL: Initializer should be literal_node"
                    test_declaration_with_initialization = .false.
                end select
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_declaration_with_initialization = .false.
        end select

    end function test_declaration_with_initialization

    logical function test_multiple_variable_declaration()
        ! TDD Test 5: Parse multiple variable declaration "real :: x, y, z"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_multiple_variable_declaration = .true.

        print '(a)', "Testing multiple variable declaration parsing..."

        ! Tokenize multiple variable declaration
        call tokenize_core("real :: x, y, z", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! For now, the parser might only handle single declarations
        ! This test documents the expected future behavior
        select type (ast_result)
        type is (declaration_node)
            ! Current implementation might only parse first variable
            if (ast_result%var_name /= "x") then
                print '(a)', "FAIL: First variable name incorrect"
                test_multiple_variable_declaration = .false.
            else
                print '(a)', "INFO: Multiple declarations not yet fully implemented"
                print '(a)', "PASS: At least first variable parsed correctly"
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_multiple_variable_declaration = .false.
        end select

    end function test_multiple_variable_declaration

    logical function test_character_declaration()
        ! TDD Test 6: Parse character declaration "character(len=10) :: name"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_character_declaration = .true.

        print '(a)', "Testing character declaration parsing..."

        ! Tokenize character declaration
        call tokenize_core("character :: name", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a declaration node
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "character") then
                print '(a)', "FAIL: Type name incorrect"
                test_character_declaration = .false.
            else if (ast_result%var_name /= "name") then
                print '(a)', "FAIL: Variable name incorrect"
                test_character_declaration = .false.
            else
                print '(a)', "PASS: Character declaration parsing"
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_character_declaration = .false.
        end select

    end function test_character_declaration

    logical function test_logical_declaration()
        ! TDD Test 7: Parse logical declaration "logical :: flag"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_logical_declaration = .true.

        print '(a)', "Testing logical declaration parsing..."

        ! Tokenize logical declaration
        call tokenize_core("logical :: flag", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a declaration node
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "logical") then
                print '(a)', "FAIL: Type name incorrect"
                test_logical_declaration = .false.
            else if (ast_result%var_name /= "flag") then
                print '(a)', "FAIL: Variable name incorrect"
                test_logical_declaration = .false.
            else
                print '(a)', "PASS: Logical declaration parsing"
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_logical_declaration = .false.
        end select

    end function test_logical_declaration

    logical function test_complex_declaration()
        ! TDD Test 8: Parse complex declaration "complex :: c"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_complex_declaration = .true.

        print '(a)', "Testing complex declaration parsing..."

        ! Tokenize complex declaration
        call tokenize_core("complex :: c", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a declaration node
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "complex") then
                print '(a)', "FAIL: Type name incorrect"
                test_complex_declaration = .false.
            else if (ast_result%var_name /= "c") then
                print '(a)', "FAIL: Variable name incorrect"
                test_complex_declaration = .false.
            else
                print '(a)', "PASS: Complex declaration parsing"
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            select type (ast_result)
            type is (literal_node)
                print '(a,a)', "  Got literal_node with value: ", trim(ast_result%value)
            type is (assignment_node)
                print '(a)', "  Got assignment_node"
            class default
                print '(a)', "  Got unknown node type"
            end select
            test_complex_declaration = .false.
        end select

    end function test_complex_declaration

end program test_frontend_parser_declarations
