program test_frontend_parser_ast_json
    use json_reader, only: json_to_ast
    use json_module
    use ast_core
    use ast_lazy_fortran
    use codegen_core, only: generate_code_polymorphic
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    ! Test 1: Read AST from JSON and generate code
    call test_ast_from_json_assignment()
    
    ! Test 2: Test with more complex AST
    call test_ast_from_json_binary_op()
    
    if (all_tests_passed) then
        print *, "All AST JSON tests passed!"
        stop 0
    else
        print *, "Some AST JSON tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_ast_from_json_assignment()
        type(json_file) :: json
        class(ast_node), allocatable :: ast
        character(len=:), allocatable :: code
        character(len=*), parameter :: json_str = &
            '{' // &
            '  "": {' // &
            '    "type": "lf_program",' // &
            '    "name": "main",' // &
            '    "implicit": true,' // &
            '    "auto_contains": false,' // &
            '    "line": 1,' // &
            '    "column": 1,' // &
            '    "body": [' // &
            '      {' // &
            '        "type": "assignment",' // &
            '        "line": 1,' // &
            '        "column": 1,' // &
            '        "target": {' // &
            '          "type": "identifier",' // &
            '          "name": "x",' // &
            '          "line": 1,' // &
            '          "column": 1' // &
            '        },' // &
            '        "value": {' // &
            '          "type": "literal",' // &
            '          "value": "42",' // &
            '          "kind": "integer",' // &
            '          "line": 1,' // &
            '          "column": 5' // &
            '        }' // &
            '      }' // &
            '    ]' // &
            '  }' // &
            '}'
        
        print *, "Testing: AST from JSON - simple assignment"
        
        ! Load JSON from string
        call json%initialize()
        call json%deserialize(json_str)
        
        ! Convert JSON to AST
        ast = json_to_ast(json)
        
        ! Check result
        if (.not. allocated(ast)) then
            print *, "FAIL: No AST created from JSON"
            all_tests_passed = .false.
            return
        end if
        
        ! Generate code from AST
        code = generate_code_polymorphic(ast)
        
        print *, "Generated code: '", code, "'"
        
        ! Check if it's a program node
        select type (ast)
        type is (lf_program_node)
            print *, "PASS: Got lf_program node"
            if (size(ast%body) == 1) then
                print *, "PASS: Program has 1 statement"
                
                ! Check the assignment
                select type (stmt => ast%body(1)%node)
                type is (assignment_node)
                    print *, "PASS: First statement is assignment"
                class default
                    print *, "FAIL: First statement is not assignment"
                    all_tests_passed = .false.
                end select
            else
                print *, "FAIL: Expected 1 statement, got ", size(ast%body)
                all_tests_passed = .false.
            end if
        class default
            print *, "FAIL: Expected lf_program node"
            all_tests_passed = .false.
        end select
        
        ! Clean up
        call json%destroy()
        
    end subroutine test_ast_from_json_assignment
    
    subroutine test_ast_from_json_binary_op()
        type(json_file) :: json
        class(ast_node), allocatable :: ast
        character(len=*), parameter :: json_str = &
            '{' // &
            '  "type": "binary_op",' // &
            '  "operator": "+",' // &
            '  "line": 1,' // &
            '  "column": 3,' // &
            '  "left": {' // &
            '    "type": "identifier",' // &
            '    "name": "a",' // &
            '    "line": 1,' // &
            '    "column": 1' // &
            '  },' // &
            '  "right": {' // &
            '    "type": "literal",' // &
            '    "value": "2",' // &
            '    "kind": "integer",' // &
            '    "line": 1,' // &
            '    "column": 5' // &
            '  }' // &
            '}'
        
        print *, ""
        print *, "Testing: AST from JSON - binary operation"
        
        ! Load JSON from string
        call json%initialize()
        call json%deserialize(json_str)
        
        ! Convert JSON to AST
        ast = json_to_ast(json)
        
        ! Check result
        if (.not. allocated(ast)) then
            print *, "FAIL: No AST created from JSON"
            all_tests_passed = .false.
            return
        end if
        
        select type (ast)
        type is (binary_op_node)
            print *, "PASS: Got binary_op node"
            if (ast%operator == "+") then
                print *, "PASS: Operator is '+'"
            else
                print *, "FAIL: Wrong operator: ", ast%operator
                all_tests_passed = .false.
            end if
            
            ! Check left operand
            if (allocated(ast%left)) then
                select type (left => ast%left)
                type is (identifier_node)
                    if (left%name == "a") then
                        print *, "PASS: Left operand is 'a'"
                    else
                        print *, "FAIL: Wrong left operand"
                        all_tests_passed = .false.
                    end if
                class default
                    print *, "FAIL: Left is not identifier"
                    all_tests_passed = .false.
                end select
            end if
            
            ! Check right operand
            if (allocated(ast%right)) then
                select type (right => ast%right)
                type is (literal_node)
                    if (right%value == "2") then
                        print *, "PASS: Right operand is '2'"
                    else
                        print *, "FAIL: Wrong right operand"
                        all_tests_passed = .false.
                    end if
                class default
                    print *, "FAIL: Right is not literal"
                    all_tests_passed = .false.
                end select
            end if
            
        class default
            print *, "FAIL: Expected binary_op node"
            all_tests_passed = .false.
        end select
        
        ! Clean up
        call json%destroy()
        
    end subroutine test_ast_from_json_binary_op
    
end program test_frontend_parser_ast_json