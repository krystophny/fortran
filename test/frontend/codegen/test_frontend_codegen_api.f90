program test_frontend_codegen_api
    use codegen_core, only: generate_code
    use ast_core
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== Code Generation API Unit Tests ==='
    print *
    
    ! Test individual code generation features via API
    if (.not. test_assignment_codegen()) all_passed = .false.
    if (.not. test_literal_codegen()) all_passed = .false.
    if (.not. test_binary_operation_codegen()) all_passed = .false.
    if (.not. test_print_statement_codegen()) all_passed = .false.
    if (.not. test_program_codegen()) all_passed = .false.
    if (.not. test_identifier_codegen()) all_passed = .false.
    
    ! Report results
    print *
    if (all_passed) then
        print *, 'All code generation API tests passed!'
        stop 0
    else
        print *, 'Some code generation API tests failed!'
        stop 1
    end if
    
contains

    logical function test_assignment_codegen()
        test_assignment_codegen = .true.
        print *, 'Testing assignment code generation...'
        
        block
            type(assignment_node) :: assign
            type(identifier_node), allocatable :: target
            type(literal_node), allocatable :: lit_value
            character(len=:), allocatable :: code
            
            allocate(target)
            allocate(lit_value)
            
            target%name = 'x'
            lit_value%value = '42'
            lit_value%literal_kind = LITERAL_INTEGER
            
            allocate(assign%target, source=target)
            allocate(assign%value, source=lit_value)
            
            code = generate_code(assign)
            
            if (len_trim(code) > 0) then
                print *, '  PASS: Assignment code generation'
                print *, '    Generated:', trim(code)
            else
                print *, '  FAIL: Assignment code generation produced empty output'
                test_assignment_codegen = .false.
            end if
        end block
        
    end function test_assignment_codegen

    logical function test_literal_codegen()
        test_literal_codegen = .true.
        print *, 'Testing literal code generation...'
        
        ! Integer literal
        block
            type(literal_node) :: lit
            character(len=:), allocatable :: code
            
            lit%value = '123'
            lit%literal_kind = LITERAL_INTEGER
            
            code = generate_code(lit)
            
            if (len_trim(code) > 0) then
                print *, '  PASS: Integer literal code generation'
            else
                print *, '  FAIL: Integer literal code generation'
                test_literal_codegen = .false.
                return
            end if
        end block
        
        ! Real literal
        block
            type(literal_node) :: lit
            character(len=:), allocatable :: code
            
            lit%value = '3.14'
            lit%literal_kind = LITERAL_REAL
            
            code = generate_code(lit)
            
            if (len_trim(code) > 0) then
                print *, '  PASS: Real literal code generation'
            else
                print *, '  FAIL: Real literal code generation'
                test_literal_codegen = .false.
                return
            end if
        end block
        
    end function test_literal_codegen

    logical function test_binary_operation_codegen()
        test_binary_operation_codegen = .true.
        print *, 'Testing binary operation code generation...'
        
        block
            type(binary_op_node) :: binop
            type(identifier_node), allocatable :: left, right
            character(len=:), allocatable :: code
            
            allocate(left)
            allocate(right)
            
            left%name = 'a'
            right%name = 'b'
            binop%operator = '+'
            
            allocate(binop%left, source=left)
            allocate(binop%right, source=right)
            
            code = generate_code(binop)
            
            if (len_trim(code) > 0) then
                print *, '  PASS: Binary operation code generation'
                print *, '    Generated:', trim(code)
            else
                print *, '  FAIL: Binary operation code generation'
                test_binary_operation_codegen = .false.
            end if
        end block
        
    end function test_binary_operation_codegen

    logical function test_print_statement_codegen()
        test_print_statement_codegen = .true.
        print *, 'Testing print statement code generation...'
        
        block
            type(print_statement_node) :: print_stmt
            type(identifier_node), allocatable :: arg
            character(len=:), allocatable :: code
            
            allocate(arg)
            arg%name = 'result'
            
            allocate(print_stmt%args(1))
            allocate(print_stmt%args(1)%node, source=arg)
            
            code = generate_code(print_stmt)
            
            if (len_trim(code) > 0) then
                print *, '  PASS: Print statement code generation'
                print *, '    Generated:', trim(code)
            else
                print *, '  FAIL: Print statement code generation'
                test_print_statement_codegen = .false.
            end if
        end block
        
    end function test_print_statement_codegen

    logical function test_program_codegen()
        test_program_codegen = .true.
        print *, 'Testing program code generation...'
        
        block
            type(program_node) :: prog
            character(len=:), allocatable :: code
            
            prog%name = 'test_program'
            allocate(prog%body(1))
            
            ! Add simple assignment
            block
                type(assignment_node), allocatable :: assign
                type(identifier_node), allocatable :: target
                type(literal_node), allocatable :: lit_value
                
                allocate(assign)
                allocate(target)
                allocate(lit_value)
                
                target%name = 'answer'
                lit_value%value = '42'
                lit_value%literal_kind = LITERAL_INTEGER
                
                allocate(assign%target, source=target)
                allocate(assign%value, source=lit_value)
                
                allocate(prog%body(1)%node, source=assign)
            end block
            
            code = generate_code(prog)
            
            if (len_trim(code) > 0) then
                print *, '  PASS: Program code generation'
                print *, '    Generated length:', len(code), 'characters'
            else
                print *, '  FAIL: Program code generation'
                test_program_codegen = .false.
            end if
        end block
        
    end function test_program_codegen

    logical function test_identifier_codegen()
        test_identifier_codegen = .true.
        print *, 'Testing identifier code generation...'
        
        block
            type(identifier_node) :: ident
            character(len=:), allocatable :: code
            
            ident%name = 'variable_name'
            
            code = generate_code(ident)
            
            if (len_trim(code) > 0) then
                print *, '  PASS: Identifier code generation'
                print *, '    Generated:', trim(code)
            else
                print *, '  FAIL: Identifier code generation'
                test_identifier_codegen = .false.
            end if
        end block
        
    end function test_identifier_codegen

end program test_frontend_codegen_api