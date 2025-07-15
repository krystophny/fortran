program test_json_workflow
    use json_reader
    use json_module
    use lexer_core
    use parser_core
    use ast_core
    use codegen_core
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    ! Test 1: Full workflow - tokens -> parser -> AST -> code
    call test_json_to_parser_workflow()
    
    ! Test 2: Full workflow - AST -> code
    call test_json_to_codegen_workflow()
    
    if (all_tests_passed) then
        print *, "All JSON workflow tests passed!"
        stop 0
    else
        print *, "Some JSON workflow tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_json_to_parser_workflow()
        type(json_file) :: json
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast
        character(len=:), allocatable :: code
        character(len=*), parameter :: json_tokens = &
            '{' // &
            '  "tokens": [' // &
            '    {"type": "identifier", "text": "y", "line": 1, "column": 1},' // &
            '    {"type": "operator", "text": "=", "line": 1, "column": 3},' // &
            '    {"type": "number", "text": "5.5", "line": 1, "column": 5},' // &
            '    {"type": "eof", "text": "", "line": 1, "column": 8}' // &
            '  ]' // &
            '}'
        
        print *, "Testing: JSON tokens -> parser -> AST -> code"
        
        ! Load JSON
        call json%initialize()
        call json%deserialize(json_tokens)
        
        ! Convert to tokens
        tokens = json_to_tokens(json)
        print *, "- Loaded ", size(tokens), " tokens from JSON"
        
        ! Parse to AST
        ast = parse_statement(tokens)
        print *, "- Parsed tokens to AST"
        
        ! Generate code
        if (allocated(ast)) then
            code = generate_code_polymorphic(ast)
            print *, "- Generated code: '", code, "'"
            
            if (code == "y = 5.5d0") then
                print *, "PASS: Complete JSON -> tokens -> parser -> code workflow"
            else
                print *, "FAIL: Expected 'y = 5.5d0'"
                all_tests_passed = .false.
            end if
        else
            print *, "FAIL: No AST created"
            all_tests_passed = .false.
        end if
        
        call json%destroy()
        
    end subroutine test_json_to_parser_workflow
    
    subroutine test_json_to_codegen_workflow()
        type(json_file) :: json
        class(ast_node), allocatable :: ast
        character(len=:), allocatable :: code
        character(len=*), parameter :: json_ast = &
            '{' // &
            '  "": {' // &
            '    "type": "lf_program",' // &
            '    "name": "test",' // &
            '    "implicit": true,' // &
            '    "auto_contains": false,' // &
            '    "line": 1,' // &
            '    "column": 1,' // &
            '    "body": [' // &
            '      {' // &
            '        "type": "assignment",' // &
            '        "line": 2,' // &
            '        "column": 1,' // &
            '        "target": {' // &
            '          "type": "identifier",' // &
            '          "name": "result",' // &
            '          "line": 2,' // &
            '          "column": 1' // &
            '        },' // &
            '        "value": {' // &
            '          "type": "binary_op",' // &
            '          "operator": "*",' // &
            '          "line": 2,' // &
            '          "column": 11,' // &
            '          "left": {' // &
            '            "type": "identifier",' // &
            '            "name": "x",' // &
            '            "line": 2,' // &
            '            "column": 10' // &
            '          },' // &
            '          "right": {' // &
            '            "type": "literal",' // &
            '            "value": "2",' // &
            '            "kind": "integer",' // &
            '            "line": 2,' // &
            '            "column": 14' // &
            '          }' // &
            '        }' // &
            '      }' // &
            '    ]' // &
            '  }' // &
            '}'
        
        print *, ""
        print *, "Testing: JSON AST -> codegen"
        
        ! Load JSON
        call json%initialize()
        call json%deserialize(json_ast)
        
        ! Convert to AST
        ast = json_to_ast(json)
        print *, "- Loaded AST from JSON"
        
        ! Generate code
        if (allocated(ast)) then
            code = generate_code_polymorphic(ast)
            print *, "- Generated code:"
            print *, code
            
            ! Check if code contains expected elements
            if (index(code, "program test") > 0 .and. &
                index(code, "result = x * 2") > 0) then
                print *, "PASS: JSON AST -> code generation workflow"
            else
                print *, "FAIL: Generated code doesn't match expected"
                all_tests_passed = .false.
            end if
        else
            print *, "FAIL: No AST created from JSON"
            all_tests_passed = .false.
        end if
        
        call json%destroy()
        
    end subroutine test_json_to_codegen_workflow
    
end program test_json_workflow