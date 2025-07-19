program test_frontend_parser_derived_types
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for derived type definitions
    if (.not. test_simple_derived_type()) all_passed = .false.
    if (.not. test_derived_type_with_components()) all_passed = .false.
    if (.not. test_derived_type_with_kind()) all_passed = .false.
    if (.not. test_nested_derived_type()) all_passed = .false.
    if (.not. test_derived_type_with_arrays()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All derived type parser tests passed"
        stop 0
    else
        print '(a)', "Some derived type parser tests failed"
        stop 1
    end if

contains

    logical function test_simple_derived_type()
        ! TDD Test 1: Parse simple derived type "type :: point"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_simple_derived_type = .true.

        print '(a)', "Testing simple derived type definition parsing..."

        ! Tokenize simple derived type
        call tokenize_core("type :: point", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a proper derived type node
        select type (ast_result)
        type is (derived_type_node)
            if (ast_result%name /= "point") then
                print '(a)', "FAIL: Type name incorrect, expected 'point', got '", trim(ast_result%name), "'"
                test_simple_derived_type = .false.
            else if (ast_result%has_parameters) then
                print '(a)', "FAIL: Should not have parameters"
                test_simple_derived_type = .false.
            else
                print '(a)', "PASS: Simple derived type definition parsing"
            end if
        class default
            print '(a)', "FAIL: Expected derived_type_node"
            test_simple_derived_type = .false.
        end select

    end function test_simple_derived_type

    logical function test_derived_type_with_components()
        ! TDD Test 2: Parse derived type without :: separator
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_derived_type_with_components = .true.

        print '(a)', "Testing derived type without :: separator parsing..."

        ! Test "type point" (without ::)
        call tokenize_core("type point", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a proper derived type node
        select type (ast_result)
        type is (derived_type_node)
            if (ast_result%name /= "point") then
                print '(a)', "FAIL: Type name incorrect, expected 'point', got '", trim(ast_result%name), "'"
                test_derived_type_with_components = .false.
            else if (ast_result%has_parameters) then
                print '(a)', "FAIL: Should not have parameters"
                test_derived_type_with_components = .false.
            else
                print '(a)', "PASS: Simple derived type without :: parsing"
            end if
        class default
            print '(a)', "FAIL: Expected derived_type_node"
            test_derived_type_with_components = .false.
        end select

    end function test_derived_type_with_components

    logical function test_derived_type_with_kind()
        ! TDD Test 3: Parse derived type with kind parameters
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_derived_type_with_kind = .true.

        print '(a)', "Testing derived type with kind parameters parsing..."

        ! Test parameterized derived type
        call tokenize_core("type :: matrix(n, m)", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a proper derived type node with parameters
        select type (ast_result)
        type is (derived_type_node)
            if (ast_result%name /= "matrix") then
                print '(a)', "FAIL: Type name incorrect, expected 'matrix', got '", trim(ast_result%name), "'"
                test_derived_type_with_kind = .false.
            else if (.not. ast_result%has_parameters) then
                print '(a)', "FAIL: Should have parameters"
                test_derived_type_with_kind = .false.
            else if (.not. allocated(ast_result%parameters)) then
                print '(a)', "FAIL: Parameters not allocated"
                test_derived_type_with_kind = .false.
            else if (size(ast_result%parameters) /= 2) then
        print '(a,i0)', "FAIL: Expected 2 parameters, got ", size(ast_result%parameters)
                test_derived_type_with_kind = .false.
            else
                print '(a)', "PASS: Parameterized derived type parsing"
            end if
        class default
            print '(a)', "FAIL: Expected derived_type_node"
            test_derived_type_with_kind = .false.
        end select

    end function test_derived_type_with_kind

    logical function test_nested_derived_type()
        ! TDD Test 4: Parse variable declaration using derived type
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_nested_derived_type = .true.

        print '(a)', "Testing variable declaration with derived type parsing..."

        ! Test declaration using derived type
        call tokenize_core("type(point) :: p", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a proper declaration node
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "type(point)") then
                print '(a)', "FAIL: Type name incorrect, expected 'type(point)', got '", trim(ast_result%type_name), "'"
                test_nested_derived_type = .false.
            else if (ast_result%var_name /= "p") then
                print '(a)', "FAIL: Variable name incorrect, expected 'p', got '", trim(ast_result%var_name), "'"
                test_nested_derived_type = .false.
            else
                print '(a)', "PASS: Variable declaration with derived type parsing"
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_nested_derived_type = .false.
        end select

    end function test_nested_derived_type

    logical function test_derived_type_with_arrays()
        ! TDD Test 5: Parse array declaration using derived type
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_derived_type_with_arrays = .true.

        print '(a)', "Testing array declaration with derived type parsing..."

        ! Test derived type usage with arrays
        call tokenize_core("type(point) :: points(10)", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a proper declaration node with array support
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "type(point)") then
                print '(a)', "FAIL: Type name incorrect, expected 'type(point)', got '", trim(ast_result%type_name), "'"
                test_derived_type_with_arrays = .false.
            else if (ast_result%var_name /= "points") then
                print '(a)', "FAIL: Variable name incorrect, expected 'points', got '", trim(ast_result%var_name), "'"
                test_derived_type_with_arrays = .false.
            else if (.not. ast_result%is_array) then
                print '(a)', "FAIL: Should be recognized as array"
                test_derived_type_with_arrays = .false.
            else if (.not. allocated(ast_result%dimensions)) then
                print '(a)', "FAIL: Array dimensions not allocated"
                test_derived_type_with_arrays = .false.
            else if (size(ast_result%dimensions) /= 1) then
         print '(a,i0)', "FAIL: Expected 1 dimension, got ", size(ast_result%dimensions)
                test_derived_type_with_arrays = .false.
            else
                print '(a)', "PASS: Array declaration with derived type parsing"
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_derived_type_with_arrays = .false.
        end select

    end function test_derived_type_with_arrays

end program test_frontend_parser_derived_types
