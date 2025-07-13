program test_parser_binary_ops
    use lexer
    use parser
    use ast
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    ! Run binary operation parsing tests
    if (.not. test_simple_addition()) all_passed = .false.
    if (.not. test_multiplication_precedence()) all_passed = .false.
    if (.not. test_parentheses_precedence()) all_passed = .false.
    if (.not. test_comparison_operators()) all_passed = .false.
    
    ! Report results
    if (all_passed) then
        print '(a)', "All binary operation parsing tests passed"
        stop 0
    else
        print '(a)', "Some binary operation parsing tests failed"
        stop 1
    end if

contains

    logical function test_simple_addition()
        type(token), allocatable :: tokens(:)
        class(ast_node), allocatable :: result
        
        test_simple_addition = .true.
        print '(a)', "Testing simple addition parsing..."
        
        ! Parse "a + b"
        call tokenize("a + b", tokens)
        
        result = parse_expression(tokens)
        
        ! Should be a binary operation node
        select type (result)
        type is (binary_op_node)
            if (result%operator /= "+") then
                print '(a)', "FAIL: Wrong operator in binary expression"
                test_simple_addition = .false.
                return
            end if
            
            ! Check left operand is identifier 'a'
            select type (left => result%left)
            type is (identifier_node)
                if (left%name /= "a") then
                    print '(a)', "FAIL: Wrong left operand, expected 'a' got '" // trim(left%name) // "'"
                    test_simple_addition = .false.
                    return
                end if
            class default
                print '(a)', "FAIL: Left operand should be identifier"
                test_simple_addition = .false.
                return
            end select
            
            ! Check right operand is identifier 'b'
            select type (right => result%right)
            type is (identifier_node)
                if (right%name /= "b") then
                    print '(a)', "FAIL: Wrong right operand in binary expression"
                    test_simple_addition = .false.
                    return
                end if
            class default
                print '(a)', "FAIL: Right operand should be identifier"
                test_simple_addition = .false.
                return
            end select
            
        class default
            print '(a)', "FAIL: Expected binary operation node"
            test_simple_addition = .false.
            return
        end select
        
        print '(a)', "PASS: Simple addition parsing"
    end function test_simple_addition

    logical function test_multiplication_precedence()
        type(token), allocatable :: tokens(:)
        class(ast_node), allocatable :: result
        
        test_multiplication_precedence = .true.
        print '(a)', "Testing multiplication precedence parsing..."
        
        ! Parse "a + b * c" which should be parsed as "a + (b * c)"
        call tokenize("a + b * c", tokens)
        
        result = parse_expression(tokens)
        
        ! Should be a binary operation node with + operator
        select type (result)
        type is (binary_op_node)
            if (result%operator /= "+") then
                print '(a)', "FAIL: Top operator should be +"
                test_multiplication_precedence = .false.
                return
            end if
            
            ! Left should be identifier 'a'
            select type (left => result%left)
            type is (identifier_node)
                if (left%name /= "a") then
                    print '(a)', "FAIL: Left operand should be 'a'"
                    test_multiplication_precedence = .false.
                    return
                end if
            class default
                print '(a)', "FAIL: Left operand should be identifier"
                test_multiplication_precedence = .false.
                return
            end select
            
            ! Right should be binary operation "b * c"
            select type (right => result%right)
            type is (binary_op_node)
                if (right%operator /= "*") then
                    print '(a)', "FAIL: Right operand should be * operation"
                    test_multiplication_precedence = .false.
                    return
                end if
            class default
                print '(a)', "FAIL: Right operand should be binary operation"
                test_multiplication_precedence = .false.
                return
            end select
            
        class default
            print '(a)', "FAIL: Expected binary operation node"
            test_multiplication_precedence = .false.
            return
        end select
        
        print '(a)', "PASS: Multiplication precedence parsing"
    end function test_multiplication_precedence

    logical function test_parentheses_precedence()
        type(token), allocatable :: tokens(:)
        class(ast_node), allocatable :: result
        
        test_parentheses_precedence = .true.
        print '(a)', "Testing parentheses precedence parsing..."
        
        ! Parse "(a + b) * c" 
        call tokenize("(a + b) * c", tokens)
        
        result = parse_expression(tokens)
        
        ! Should be a binary operation node with * operator
        select type (result)
        type is (binary_op_node)
            if (result%operator /= "*") then
                print '(a)', "FAIL: Top operator should be *"
                test_parentheses_precedence = .false.
                return
            end if
            
            ! Left should be binary operation "a + b"
            select type (left => result%left)
            type is (binary_op_node)
                if (left%operator /= "+") then
                    print '(a)', "FAIL: Left operand should be + operation"
                    test_parentheses_precedence = .false.
                    return
                end if
            class default
                print '(a)', "FAIL: Left operand should be binary operation"
                test_parentheses_precedence = .false.
                return
            end select
            
            ! Right should be identifier 'c'
            select type (right => result%right)
            type is (identifier_node)
                if (right%name /= "c") then
                    print '(a)', "FAIL: Right operand should be 'c'"
                    test_parentheses_precedence = .false.
                    return
                end if
            class default
                print '(a)', "FAIL: Right operand should be identifier"
                test_parentheses_precedence = .false.
                return
            end select
            
        class default
            print '(a)', "FAIL: Expected binary operation node"
            test_parentheses_precedence = .false.
            return
        end select
        
        print '(a)', "PASS: Parentheses precedence parsing"
    end function test_parentheses_precedence

    logical function test_comparison_operators()
        type(token), allocatable :: tokens(:)
        class(ast_node), allocatable :: result
        
        test_comparison_operators = .true.
        print '(a)', "Testing comparison operator parsing..."
        
        ! Parse "x == 42"
        call tokenize("x == 42", tokens)
        
        result = parse_expression(tokens)
        
        ! Should be a binary operation node with == operator
        select type (result)
        type is (binary_op_node)
            if (result%operator /= "==") then
                print '(a)', "FAIL: Wrong comparison operator"
                test_comparison_operators = .false.
                return
            end if
            
            ! Check left operand is identifier 'x'
            select type (left => result%left)
            type is (identifier_node)
                if (left%name /= "x") then
                    print '(a)', "FAIL: Wrong left operand"
                    test_comparison_operators = .false.
                    return
                end if
            class default
                print '(a)', "FAIL: Left operand should be identifier"
                test_comparison_operators = .false.
                return
            end select
            
            ! Check right operand is literal '42'
            select type (right => result%right)
            type is (literal_node)
                if (right%value /= "42") then
                    print '(a)', "FAIL: Wrong right operand"
                    test_comparison_operators = .false.
                    return
                end if
            class default
                print '(a)', "FAIL: Right operand should be literal"
                test_comparison_operators = .false.
                return
            end select
            
        class default
            print '(a)', "FAIL: Expected binary operation node"
            test_comparison_operators = .false.
            return
        end select
        
        print '(a)', "PASS: Comparison operator parsing"
    end function test_comparison_operators

end program test_parser_binary_ops