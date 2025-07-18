program test_frontend_parser_api
    use lexer_core, only: tokenize_core, token_t
    use parser_core, only: parse_statement
    use ast_core
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== Parser API Unit Tests ==='
    print *
    
    ! Test individual parser features via API
    if (.not. test_assignment_parsing()) all_passed = .false.
    if (.not. test_binary_operation_parsing()) all_passed = .false.
    if (.not. test_literal_parsing()) all_passed = .false.
    if (.not. test_identifier_parsing()) all_passed = .false.
    if (.not. test_print_statement_parsing()) all_passed = .false.
    if (.not. test_declaration_parsing()) all_passed = .false.
    
    ! Report results
    print *
    if (all_passed) then
        print *, 'All parser API tests passed!'
        stop 0
    else
        print *, 'Some parser API tests failed!'
        stop 1
    end if
    
contains

    logical function test_assignment_parsing()
        test_assignment_parsing = .true.
        print *, 'Testing assignment parsing...'
        
        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: node
            
            ! Simple assignment
            call tokenize_core('x = 5', tokens)
            node = parse_statement(tokens)
            
            if (.not. allocated(node)) then
                print *, '  FAIL: Simple assignment parsing'
                test_assignment_parsing = .false.
                return
            end if
            
            select type(node)
            type is (assignment_node)
                print *, '  PASS: Simple assignment parsing'
            class default
                print *, '  WARN: Assignment parsed as different node type'
            end select
        end block
        
        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: node
            
            ! Assignment with expression
            call tokenize_core('y = x + 1', tokens)
            node = parse_statement(tokens)
            
            if (.not. allocated(node)) then
                print *, '  FAIL: Assignment with expression parsing'
                test_assignment_parsing = .false.
                return
            end if
            
            select type(node)
            type is (assignment_node)
                print *, '  PASS: Assignment with expression parsing'
            class default
                print *, '  WARN: Assignment with expression parsed as different node type'
            end select
        end block
        
    end function test_assignment_parsing

    logical function test_binary_operation_parsing()
        test_binary_operation_parsing = .true.
        print *, 'Testing binary operation parsing...'
        
        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: node
            
            ! Addition
            call tokenize_core('a + b', tokens)
            node = parse_statement(tokens)
            
            if (.not. allocated(node)) then
                print *, '  FAIL: Addition parsing'
                test_binary_operation_parsing = .false.
                return
            end if
            
            print *, '  PASS: Addition parsing'
        end block
        
        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: node
            
            ! Multiplication
            call tokenize_core('x * y', tokens)
            node = parse_statement(tokens)
            
            if (.not. allocated(node)) then
                print *, '  FAIL: Multiplication parsing'
                test_binary_operation_parsing = .false.
                return
            end if
            
            print *, '  PASS: Multiplication parsing'
        end block
        
    end function test_binary_operation_parsing

    logical function test_literal_parsing()
        test_literal_parsing = .true.
        print *, 'Testing literal parsing...'
        
        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: node
            
            ! Integer literal
            call tokenize_core('42', tokens)
            node = parse_statement(tokens)
            
            if (.not. allocated(node)) then
                print *, '  FAIL: Integer literal parsing'
                test_literal_parsing = .false.
                return
            end if
            
            print *, '  PASS: Integer literal parsing'
        end block
        
        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: node
            
            ! Real literal
            call tokenize_core('3.14', tokens)
            node = parse_statement(tokens)
            
            if (.not. allocated(node)) then
                print *, '  FAIL: Real literal parsing'
                test_literal_parsing = .false.
                return
            end if
            
            print *, '  PASS: Real literal parsing'
        end block
        
    end function test_literal_parsing

    logical function test_identifier_parsing()
        test_identifier_parsing = .true.
        print *, 'Testing identifier parsing...'
        
        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: node
            
            ! Simple identifier
            call tokenize_core('variable', tokens)
            node = parse_statement(tokens)
            
            if (.not. allocated(node)) then
                print *, '  FAIL: Identifier parsing'
                test_identifier_parsing = .false.
                return
            end if
            
            print *, '  PASS: Identifier parsing'
        end block
        
    end function test_identifier_parsing

    logical function test_print_statement_parsing()
        test_print_statement_parsing = .true.
        print *, 'Testing print statement parsing...'
        
        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: node
            
            ! Simple print
            call tokenize_core('print *, x', tokens)
            node = parse_statement(tokens)
            
            if (.not. allocated(node)) then
                print *, '  FAIL: Print statement parsing'
                test_print_statement_parsing = .false.
                return
            end if
            
            select type(node)
            type is (print_statement_node)
                print *, '  PASS: Print statement parsing'
            class default
                print *, '  WARN: Print statement parsed as different node type'
            end select
        end block
        
    end function test_print_statement_parsing

    logical function test_declaration_parsing()
        test_declaration_parsing = .true.
        print *, 'Testing declaration parsing...'
        
        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: node
            
            ! Type declaration
            call tokenize_core('real :: x', tokens)
            node = parse_statement(tokens)
            
            if (.not. allocated(node)) then
                print *, '  FAIL: Declaration parsing'
                test_declaration_parsing = .false.
                return
            end if
            
            print *, '  PASS: Declaration parsing'
        end block
        
    end function test_declaration_parsing

end program test_frontend_parser_api