program test_parse_and_codegen
    use lexer
    use parser
    use ast
    use codegen
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    ! Run integration tests
    if (.not. test_round_trip_literal()) all_passed = .false.
    if (.not. test_round_trip_assignment()) all_passed = .false.
    if (.not. test_round_trip_expression()) all_passed = .false.
    
    ! Report results
    if (all_passed) then
        print '(a)', "All parse and codegen integration tests passed"
        stop 0
    else
        print '(a)', "Some parse and codegen integration tests failed"
        stop 1
    end if

contains

    logical function test_round_trip_literal()
        character(len=*), parameter :: source = "42"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast
        character(len=:), allocatable :: generated
        
        test_round_trip_literal = .true.
        print '(a)', "Testing literal round trip..."
        
        ! Tokenize
        call tokenize(source, tokens)
        
        ! Parse
        ast = parse_expression(tokens)
        
        ! Generate code
        select type (ast)
        type is (literal_node)
            generated = generate_code(ast)
        class default
            print '(a)', "FAIL: Expected literal node"
            test_round_trip_literal = .false.
            return
        end select
        
        ! Check round trip
        if (generated /= source) then
            print '(a)', "FAIL: Round trip failed"
            print '(a)', "  Original: '" // source // "'"
            print '(a)', "  Generated: '" // generated // "'"
            test_round_trip_literal = .false.
        else
            print '(a)', "PASS: Literal round trip"
        end if
    end function test_round_trip_literal

    logical function test_round_trip_assignment()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast
        character(len=:), allocatable :: generated
        
        test_round_trip_assignment = .true.
        print '(a)', "Testing assignment round trip..."
        
        ! Tokenize
        call tokenize(source, tokens)
        
        ! Parse
        ast = parse_statement(tokens)
        
        ! Generate code
        select type (ast)
        type is (assignment_node)
            generated = generate_code(ast)
        class default
            print '(a)', "FAIL: Expected assignment node"
            test_round_trip_assignment = .false.
            return
        end select
        
        ! Check round trip
        if (generated /= source) then
            print '(a)', "FAIL: Round trip failed"
            print '(a)', "  Original: '" // source // "'"
            print '(a)', "  Generated: '" // generated // "'"
            test_round_trip_assignment = .false.
        else
            print '(a)', "PASS: Assignment round trip"
        end if
    end function test_round_trip_assignment

    logical function test_round_trip_expression()
        character(len=*), parameter :: source = "a + b * c"
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast
        character(len=:), allocatable :: generated
        
        test_round_trip_expression = .true.
        print '(a)', "Testing expression round trip..."
        
        ! Tokenize
        call tokenize(source, tokens)
        
        ! Parse
        ast = parse_expression(tokens)
        
        ! Generate code
        select type (ast)
        type is (binary_op_node)
            generated = generate_code(ast)
        class default
            print '(a)', "FAIL: Expected binary operation node"
            test_round_trip_expression = .false.
            return
        end select
        
        ! Check round trip
        if (generated /= source) then
            print '(a)', "FAIL: Round trip failed"
            print '(a)', "  Original: '" // source // "'"
            print '(a)', "  Generated: '" // generated // "'"
            test_round_trip_expression = .false.
        else
            print '(a)', "PASS: Expression round trip"
        end if
    end function test_round_trip_expression

end program test_parse_and_codegen