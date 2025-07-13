program test_codegen_program
    use ast
    use codegen
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    ! Run program code generation tests  
    if (.not. test_statement_generation()) all_passed = .false.
    if (.not. test_use_statement()) all_passed = .false.
    if (.not. test_print_statement()) all_passed = .false.
    
    ! Report results
    if (all_passed) then
        print '(a)', "All program code generation tests passed"
        stop 0
    else
        print '(a)', "Some program code generation tests failed"
        stop 1
    end if

contains

    logical function test_statement_generation()
        type(assignment_node) :: assign
        type(identifier_node) :: target
        type(literal_node) :: value
        type(binary_op_node) :: expr
        character(len=:), allocatable :: code
        
        test_statement_generation = .true.
        print '(a)', "Testing statement code generation..."
        
        ! Create assignment with expression: z = x + y
        target = create_identifier("z", 1, 1)
        value = create_literal("10", LITERAL_INTEGER, 1, 5)
        expr = create_binary_op( &
            create_identifier("x", 1, 5), &
            create_identifier("y", 1, 9), &
            "+", 1, 7)
        assign = create_assignment(target, expr, 1, 1)
        
        ! Generate code
        code = generate_code(assign)
        
        ! Check generated code
        if (code /= "z = x + y") then
            print '(a)', "FAIL: Statement generation incorrect"
            print '(a)', "  Expected: 'z = x + y'"
            print '(a)', "  Got: '" // code // "'"
            test_statement_generation = .false.
        else
            print '(a)', "PASS: Statement generation"
        end if
    end function test_statement_generation

    logical function test_use_statement()
        type(use_statement_node) :: use_stmt
        character(len=:), allocatable :: code
        
        test_use_statement = .true.
        print '(a)', "Testing use statement generation..."
        
        ! Create use statement
        use_stmt = create_use_statement("iso_fortran_env", line=1, column=1)
        
        ! Generate code
        code = generate_code(use_stmt)
        
        ! Check generated code
        if (code /= "use iso_fortran_env") then
            print '(a)', "FAIL: Use statement generation incorrect"
            print '(a)', "  Expected: 'use iso_fortran_env'"
            print '(a)', "  Got: '" // code // "'"
            test_use_statement = .false.
        else
            print '(a)', "PASS: Use statement generation"
        end if
    end function test_use_statement

    logical function test_print_statement()
        type(print_statement_node) :: print_stmt
        type(identifier_node) :: var
        class(ast_node), allocatable :: args(:)
        character(len=:), allocatable :: code
        
        test_print_statement = .true.
        print '(a)', "Testing print statement generation..."
        
        ! Create print statement with one argument
        var = create_identifier("result", 1, 7)
        allocate(args, source=[var])
        print_stmt = create_print_statement(args, line=1, column=1)
        
        ! Generate code
        code = generate_code(print_stmt)
        
        ! Check generated code
        if (code /= "print *, result") then
            print '(a)', "FAIL: Print statement generation incorrect"
            print '(a)', "  Expected: 'print *, result'"
            print '(a)', "  Got: '" // code // "'"
            test_print_statement = .false.
        else
            print '(a)', "PASS: Print statement generation"
        end if
    end function test_print_statement

end program test_codegen_program