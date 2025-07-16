program test_frontend_parser_assignment
    use lexer_core, only: tokenize_core, token_t
    use parser_core, only: parse_statement
    use ast_core, only: ast_node, assignment_node, identifier_node, literal_node
    implicit none
    
    type(token_t), allocatable :: tokens(:)
    class(ast_node), allocatable :: ast
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    ! Test 1: Simple assignment "x = 1"
    call test_simple_assignment()
    
    if (all_tests_passed) then
        print *, "All parser assignment tests passed!"
        stop 0
    else
        print *, "Some parser assignment tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_simple_assignment()
        character(len=*), parameter :: input = "x = 1"
        
        print *, "Testing: '", input, "'"
        
        ! First tokenize
        call tokenize_core(input, tokens)
        
        ! Then parse
        ast = parse_statement(tokens)
        
        ! Check if we got an assignment node
        if (.not. allocated(ast)) then
            print *, "FAIL: No AST node created"
            all_tests_passed = .false.
            return
        end if
        
        select type (ast)
        type is (assignment_node)
            print *, "PASS: Got assignment node"
            
            ! Check target is identifier "x"
            if (allocated(ast%target)) then
                select type (target => ast%target)
                type is (identifier_node)
                    if (target%name == "x") then
                        print *, "PASS: Target is identifier 'x'"
                    else
                        print *, "FAIL: Target name is '", target%name, "', expected 'x'"
                        all_tests_passed = .false.
                    end if
                class default
                    print *, "FAIL: Target is not an identifier"
                    all_tests_passed = .false.
                end select
            else
                print *, "FAIL: No target in assignment"
                all_tests_passed = .false.
            end if
            
            ! Check value is literal "1"
            if (allocated(ast%value)) then
                select type (value => ast%value)
                type is (literal_node)
                    if (value%value == "1") then
                        print *, "PASS: Value is literal '1'"
                    else
                        print *, "FAIL: Value is '", value%value, "', expected '1'"
                        all_tests_passed = .false.
                    end if
                class default
                    print *, "FAIL: Value is not a literal"
                    all_tests_passed = .false.
                end select
            else
                print *, "FAIL: No value in assignment"
                all_tests_passed = .false.
            end if
            
        class default
            print *, "FAIL: Expected assignment node"
            all_tests_passed = .false.
        end select
        
    end subroutine test_simple_assignment
    
end program test_frontend_parser_assignment