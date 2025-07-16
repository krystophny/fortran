program test_parser_basic
    use lexer_core
    use ast_core
    use parser_core
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    ! Run TDD tests in order
    if (.not. test_parse_simple_literal()) all_passed = .false.
    if (.not. test_parse_simple_identifier()) all_passed = .false.
    if (.not. test_parse_simple_assignment()) all_passed = .false.
    
    ! Report results
    if (all_passed) then
        print '(a)', "All basic parser tests passed"
        stop 0
    else
        print '(a)', "Some basic parser tests failed"
        stop 1
    end if

contains

    logical function test_parse_simple_literal()
        ! TDD Test 1: Parse a simple literal "42" into literal_node
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result
        
        test_parse_simple_literal = .true.
        
        print '(a)', "Testing simple literal parsing..."
        
        ! Tokenize simple literal
        call tokenize_core("42", tokens)
        
        ! Parse expression
        ast_result = parse_expression(tokens)
        
        ! Check that we got a literal node
        select type (ast_result)
        type is (literal_node)
            if (ast_result%value /= "42") then
                print '(a)', "FAIL: Literal value incorrect"
                test_parse_simple_literal = .false.
            else if (ast_result%literal_kind /= LITERAL_INTEGER) then
                print '(a)', "FAIL: Literal kind incorrect"
                test_parse_simple_literal = .false.
            else
                print '(a)', "PASS: Simple literal parsing"
            end if
        class default
            print '(a)', "FAIL: Expected literal_node"
            test_parse_simple_literal = .false.
        end select
        
    end function test_parse_simple_literal

    logical function test_parse_simple_identifier()
        ! TDD Test 2: Parse a simple identifier "x" into identifier_node
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result
        
        test_parse_simple_identifier = .true.
        
        print '(a)', "Testing simple identifier parsing..."
        
        ! Tokenize simple identifier
        call tokenize_core("x", tokens)
        
        ! Parse expression
        ast_result = parse_expression(tokens)
        
        ! Check that we got an identifier node
        select type (ast_result)
        type is (identifier_node)
            if (ast_result%name /= "x") then
                print '(a)', "FAIL: Identifier name incorrect"
                test_parse_simple_identifier = .false.
            else
                print '(a)', "PASS: Simple identifier parsing"
            end if
        class default
            print '(a)', "FAIL: Expected identifier_node"
            test_parse_simple_identifier = .false.
        end select
        
    end function test_parse_simple_identifier

    logical function test_parse_simple_assignment()
        ! TDD Test 3: Parse simple assignment "x = 42" into assignment_node
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result
        
        test_parse_simple_assignment = .true.
        
        print '(a)', "Testing simple assignment parsing..."
        
        ! Tokenize simple assignment
        call tokenize_core("x = 42", tokens)
        
        ! Parse statement
        ast_result = parse_statement(tokens)
        
        ! Check that we got an assignment node
        select type (ast_result)
        type is (assignment_node)
            ! Check target is identifier
            select type (target => ast_result%target)
            type is (identifier_node)
                if (target%name /= "x") then
                    print '(a)', "FAIL: Assignment target name incorrect"
                    test_parse_simple_assignment = .false.
                    return
                end if
            class default
                print '(a)', "FAIL: Assignment target should be identifier"
                test_parse_simple_assignment = .false.
                return
            end select
            
            ! Check value is literal
            select type (value => ast_result%value)
            type is (literal_node)
                if (value%value /= "42") then
                    print '(a)', "FAIL: Assignment value incorrect"
                    test_parse_simple_assignment = .false.
                    return
                end if
            class default
                print '(a)', "FAIL: Assignment value should be literal"
                test_parse_simple_assignment = .false.
                return
            end select
            
            print '(a)', "PASS: Simple assignment parsing"
        class default
            print '(a)', "FAIL: Expected assignment_node"
            test_parse_simple_assignment = .false.
        end select
        
    end function test_parse_simple_assignment

end program test_parser_basic