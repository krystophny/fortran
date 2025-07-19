program test_frontend_parser_array_declarations
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for array declarations
    if (.not. test_static_array_declaration()) all_passed = .false.
    if (.not. test_dynamic_array_declaration()) all_passed = .false.
    if (.not. test_assumed_shape_array()) all_passed = .false.
    if (.not. test_multidimensional_array()) all_passed = .false.
    if (.not. test_character_array()) all_passed = .false.
    if (.not. test_array_with_bounds()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All array declaration parser tests passed"
        stop 0
    else
        print '(a)', "Some array declaration parser tests failed"
        stop 1
    end if

contains

    logical function test_static_array_declaration()
        ! TDD Test 1: Parse static array declaration "real :: arr(10)"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_static_array_declaration = .true.

        print '(a)', "Testing static array declaration parsing..."

        ! Tokenize static array declaration
        call tokenize_core("real :: arr(10)", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a proper declaration node with array info
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "real") then
                print '(a)', "FAIL: Type name incorrect"
                test_static_array_declaration = .false.
            else if (ast_result%var_name /= "arr") then
                print '(a)', "FAIL: Variable name incorrect"
                test_static_array_declaration = .false.
            else if (.not. ast_result%is_array) then
                print '(a)', "FAIL: Array flag not set"
                test_static_array_declaration = .false.
            else if (.not. allocated(ast_result%dimensions)) then
                print '(a)', "FAIL: Dimensions not allocated"
                test_static_array_declaration = .false.
            else if (size(ast_result%dimensions) /= 1) then
                print '(a)', "FAIL: Wrong number of dimensions"
                test_static_array_declaration = .false.
            else
                ! Check first dimension
                if (allocated(ast_result%dimensions(1)%node)) then
                    select type (dim => ast_result%dimensions(1)%node)
                    type is (literal_node)
                        if (dim%value /= "10") then
                            print '(a)', "FAIL: Dimension value incorrect"
                            test_static_array_declaration = .false.
                        else
                            print '(a)', "PASS: Static array declaration parsing"
                        end if
                    class default
                        print '(a)', "FAIL: Dimension should be literal_node"
                        test_static_array_declaration = .false.
                    end select
                else
                    print '(a)', "FAIL: Dimension node not allocated"
                    test_static_array_declaration = .false.
                end if
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_static_array_declaration = .false.
        end select

    end function test_static_array_declaration

    logical function test_dynamic_array_declaration()
        ! TDD Test 2: Parse dynamic array declaration "real, allocatable :: arr(:)"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_dynamic_array_declaration = .true.

        print '(a)', "Testing dynamic array declaration parsing..."

        ! Tokenize dynamic array declaration
        call tokenize_core("real, allocatable :: arr(:)", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a proper declaration node with allocatable attribute
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "real") then
                print '(a)', "FAIL: Type name incorrect"
                test_dynamic_array_declaration = .false.
            else if (ast_result%var_name /= "arr") then
                print '(a)', "FAIL: Variable name incorrect"
                test_dynamic_array_declaration = .false.
            else if (.not. ast_result%is_array) then
                print '(a)', "FAIL: Array flag not set"
                test_dynamic_array_declaration = .false.
            else if (.not. ast_result%is_allocatable) then
                print '(a)', "FAIL: Allocatable flag not set"
                test_dynamic_array_declaration = .false.
            else if (.not. allocated(ast_result%dimensions)) then
                print '(a)', "FAIL: Dimensions not allocated"
                test_dynamic_array_declaration = .false.
            else if (size(ast_result%dimensions) /= 1) then
                print '(a)', "FAIL: Wrong number of dimensions"
                test_dynamic_array_declaration = .false.
            else
                ! Check first dimension is assumed shape (:)
                if (allocated(ast_result%dimensions(1)%node)) then
                    select type (dim => ast_result%dimensions(1)%node)
                    type is (literal_node)
                        if (dim%value /= ":") then
                            print '(a)', "FAIL: Dimension value incorrect, expected ':'"
                            test_dynamic_array_declaration = .false.
                        else
                            print '(a)', "PASS: Dynamic array declaration parsing"
                        end if
                    class default
                        print '(a)', "FAIL: Dimension should be literal_node"
                        test_dynamic_array_declaration = .false.
                    end select
                else
                    print '(a)', "FAIL: Dimension node not allocated"
                    test_dynamic_array_declaration = .false.
                end if
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_dynamic_array_declaration = .false.
        end select

    end function test_dynamic_array_declaration

    logical function test_assumed_shape_array()
        ! TDD Test 3: Parse assumed shape array "real :: arr(:)"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_assumed_shape_array = .true.

        print '(a)', "Testing assumed shape array declaration parsing..."

        ! Tokenize assumed shape array declaration
        call tokenize_core("real :: arr(:)", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a proper declaration node with array info
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "real") then
                print '(a)', "FAIL: Type name incorrect"
                test_assumed_shape_array = .false.
            else if (ast_result%var_name /= "arr") then
                print '(a)', "FAIL: Variable name incorrect"
                test_assumed_shape_array = .false.
            else if (.not. ast_result%is_array) then
                print '(a)', "FAIL: Array flag not set"
                test_assumed_shape_array = .false.
            else if (.not. allocated(ast_result%dimensions)) then
                print '(a)', "FAIL: Dimensions not allocated"
                test_assumed_shape_array = .false.
            else if (size(ast_result%dimensions) /= 1) then
                print '(a)', "FAIL: Wrong number of dimensions"
                test_assumed_shape_array = .false.
            else
                ! Check first dimension
                if (allocated(ast_result%dimensions(1)%node)) then
                    select type (dim => ast_result%dimensions(1)%node)
                    type is (literal_node)
                        if (dim%value /= ":") then
                            print '(a)', "FAIL: Dimension value incorrect, expected ':'"
                            test_assumed_shape_array = .false.
                        else
                            print '(a)', "PASS: Assumed shape array declaration parsing"
                        end if
                    class default
                        print '(a)', "FAIL: Dimension should be literal_node"
                        test_assumed_shape_array = .false.
                    end select
                else
                    print '(a)', "FAIL: Dimension node not allocated"
                    test_assumed_shape_array = .false.
                end if
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_assumed_shape_array = .false.
        end select

    end function test_assumed_shape_array

    logical function test_multidimensional_array()
        ! TDD Test 4: Parse multidimensional array "real :: matrix(3, 4)"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_multidimensional_array = .true.

        print '(a)', "Testing multidimensional array declaration parsing..."

        ! Tokenize multidimensional array declaration
        call tokenize_core("real :: matrix(3, 4)", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a proper declaration node with array info
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "real") then
                print '(a)', "FAIL: Type name incorrect"
                test_multidimensional_array = .false.
            else if (ast_result%var_name /= "matrix") then
                print '(a)', "FAIL: Variable name incorrect"
                test_multidimensional_array = .false.
            else if (.not. ast_result%is_array) then
                print '(a)', "FAIL: Array flag not set"
                test_multidimensional_array = .false.
            else if (.not. allocated(ast_result%dimensions)) then
                print '(a)', "FAIL: Dimensions not allocated"
                test_multidimensional_array = .false.
            else if (size(ast_result%dimensions) /= 2) then
                print '(a)', "FAIL: Wrong number of dimensions, expected 2"
                test_multidimensional_array = .false.
            else
                ! Check both dimensions
                block
                    logical :: dim1_ok, dim2_ok
                    dim1_ok = .false.
                    dim2_ok = .false.

                    if (allocated(ast_result%dimensions(1)%node)) then
                        select type (dim => ast_result%dimensions(1)%node)
                        type is (literal_node)
                            if (dim%value == "3") dim1_ok = .true.
                        end select
                    end if

                    if (allocated(ast_result%dimensions(2)%node)) then
                        select type (dim => ast_result%dimensions(2)%node)
                        type is (literal_node)
                            if (dim%value == "4") dim2_ok = .true.
                        end select
                    end if

                    if (.not. dim1_ok) then
                        print '(a)', "FAIL: First dimension incorrect"
                        test_multidimensional_array = .false.
                    else if (.not. dim2_ok) then
                        print '(a)', "FAIL: Second dimension incorrect"
                        test_multidimensional_array = .false.
                    else
                        print '(a)', "PASS: Multidimensional array declaration parsing"
                    end if
                end block
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_multidimensional_array = .false.
        end select

    end function test_multidimensional_array

    logical function test_character_array()
        ! TDD Test 5: Parse character array "character(len=20) :: names(5)"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_character_array = .true.

        print '(a)', "Testing character array declaration parsing..."

        ! Tokenize character array declaration
        call tokenize_core("character(len=20) :: names(5)", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a proper declaration node
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "character(len=20)") then
                print '(a,a)', "FAIL: Type name incorrect, expected 'character(len=20)', got: ", ast_result%type_name
                test_character_array = .false.
            else if (ast_result%var_name /= "names") then
                print '(a)', "FAIL: Variable name incorrect"
                test_character_array = .false.
            else if (.not. ast_result%is_array) then
                print '(a)', "FAIL: Array flag not set"
                test_character_array = .false.
            else if (.not. allocated(ast_result%dimensions)) then
                print '(a)', "FAIL: Dimensions not allocated"
                test_character_array = .false.
            else if (size(ast_result%dimensions) /= 1) then
                print '(a)', "FAIL: Wrong number of dimensions"
                test_character_array = .false.
            else
                ! Check dimension
                if (allocated(ast_result%dimensions(1)%node)) then
                    select type (dim => ast_result%dimensions(1)%node)
                    type is (literal_node)
                        if (dim%value /= "5") then
        print '(a,a)', "FAIL: Dimension value incorrect, expected '5', got: ", dim%value
                            test_character_array = .false.
                        else
                            print '(a)', "PASS: Character array declaration parsing"
                        end if
                    class default
                        print '(a)', "FAIL: Dimension should be literal_node"
                        test_character_array = .false.
                    end select
                else
                    print '(a)', "FAIL: Dimension node not allocated"
                    test_character_array = .false.
                end if
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_character_array = .false.
        end select

    end function test_character_array

    logical function test_array_with_bounds()
        ! TDD Test 6: Parse array with explicit bounds "real :: arr(1:10)"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_array_with_bounds = .true.

        print '(a)', "Testing array with explicit bounds parsing..."

        ! Tokenize array with bounds
        call tokenize_core("real :: arr(1:10)", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we got a proper declaration node with bounds
        select type (ast_result)
        type is (declaration_node)
            if (ast_result%type_name /= "real") then
                print '(a)', "FAIL: Type name incorrect"
                test_array_with_bounds = .false.
            else if (ast_result%var_name /= "arr") then
                print '(a)', "FAIL: Variable name incorrect"
                test_array_with_bounds = .false.
            else if (.not. ast_result%is_array) then
                print '(a)', "FAIL: Array flag not set"
                test_array_with_bounds = .false.
            else if (.not. allocated(ast_result%dimensions)) then
                print '(a)', "FAIL: Dimensions not allocated"
                test_array_with_bounds = .false.
            else if (size(ast_result%dimensions) /= 1) then
                print '(a)', "FAIL: Wrong number of dimensions"
                test_array_with_bounds = .false.
            else
                ! Check dimension is bounds notation
                if (allocated(ast_result%dimensions(1)%node)) then
                    select type (dim => ast_result%dimensions(1)%node)
                    type is (literal_node)
                        if (dim%value /= "1:10") then
     print '(a,a)', "FAIL: Dimension value incorrect, expected '1:10', got: ", dim%value
                            test_array_with_bounds = .false.
                        else
                            print '(a)', "PASS: Array with bounds declaration parsing"
                        end if
                    class default
                        print '(a)', "FAIL: Dimension should be literal_node"
                        test_array_with_bounds = .false.
                    end select
                else
                    print '(a)', "FAIL: Dimension node not allocated"
                    test_array_with_bounds = .false.
                end if
            end if
        class default
            print '(a)', "FAIL: Expected declaration_node"
            test_array_with_bounds = .false.
        end select

    end function test_array_with_bounds

end program test_frontend_parser_array_declarations
