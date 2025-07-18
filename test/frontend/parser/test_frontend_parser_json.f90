program test_frontend_parser_json
    use json_reader, only: json_to_tokens
    use json_module
    use lexer_core, only: token_t
    use parser_core, only: parse_statement
    use ast_core, only: ast_node, assignment_node, identifier_node, literal_node
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    ! Test 1: Parse assignment from JSON tokens
    call test_parse_from_json_tokens()
    
    if (all_tests_passed) then
        print *, "All parser JSON tests passed!"
        stop 0
    else
        print *, "Some parser JSON tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_parse_from_json_tokens()
        type(json_file) :: json
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast
        character(len=*), parameter :: json_str = &
            '{"tokens": [' // &
            '  {"type": "identifier", "text": "x", "line": 1, "column": 1},' // &
            '  {"type": "operator", "text": "=", "line": 1, "column": 3},' // &
            '  {"type": "number", "text": "42", "line": 1, "column": 5},' // &
            '  {"type": "eof", "text": "", "line": 1, "column": 7}' // &
            ']}'
        
        print *, "Testing: Parse assignment from JSON tokens"
        
        ! Load JSON from string
        call json%initialize()
        call json%deserialize(json_str)
        
        ! Convert JSON to tokens
        tokens = json_to_tokens(json)
        
        ! Parse tokens to AST
        ast = parse_statement(tokens)
        
        ! Check result
        if (.not. allocated(ast)) then
            print *, "FAIL: No AST created from JSON tokens"
            all_tests_passed = .false.
            return
        end if
        
        select type (ast)
        type is (assignment_node)
            print *, "PASS: Got assignment node from JSON tokens"
            
            ! Check target
            if (allocated(ast%target)) then
                select type (target => ast%target)
                type is (identifier_node)
                    if (target%name == "x") then
                        print *, "PASS: Target is 'x'"
                    else
                        print *, "FAIL: Wrong target name"
                        all_tests_passed = .false.
                    end if
                class default
                    print *, "FAIL: Target is not identifier"
                    all_tests_passed = .false.
                end select
            end if
            
            ! Check value
            if (allocated(ast%value)) then
                select type (value => ast%value)
                type is (literal_node)
                    if (value%value == "42") then
                        print *, "PASS: Value is '42'"
                    else
                        print *, "FAIL: Wrong value"
                        all_tests_passed = .false.
                    end if
                class default
                    print *, "FAIL: Value is not literal"
                    all_tests_passed = .false.
                end select
            end if
            
        class default
            print *, "FAIL: Expected assignment node from JSON"
            all_tests_passed = .false.
        end select
        
        ! Clean up
        call json%destroy()
        
    end subroutine test_parse_from_json_tokens
    
end program test_frontend_parser_json