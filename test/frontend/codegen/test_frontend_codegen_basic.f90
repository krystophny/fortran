program test_codegen_basic
    use ast_core
    use codegen_core
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    ! Run basic code generation tests
    if (.not. test_literal_codegen()) all_passed = .false.
    if (.not. test_identifier_codegen()) all_passed = .false.
    if (.not. test_assignment_codegen()) all_passed = .false.
    if (.not. test_binary_op_codegen()) all_passed = .false.
    
    ! Report results
    if (all_passed) then
        print '(a)', "All basic code generation tests passed"
        stop 0
    else
        print '(a)', "Some basic code generation tests failed"
        stop 1
    end if

contains

    logical function test_literal_codegen()
        type(literal_node) :: literal
        character(len=:), allocatable :: code
        
        test_literal_codegen = .true.
        print '(a)', "Testing literal code generation..."
        
        ! Create integer literal
        literal = create_literal("42", LITERAL_INTEGER, 1, 1)
        
        ! Generate code
        code = generate_code(literal)
        
        ! Check generated code
        if (code /= "42") then
            print '(a)', "FAIL: Integer literal code generation incorrect"
            print '(a)', "  Expected: '42'"
            print '(a)', "  Got: '" // code // "'"
            test_literal_codegen = .false.
        else
            print '(a)', "PASS: Literal code generation"
        end if
    end function test_literal_codegen

    logical function test_identifier_codegen()
        type(identifier_node) :: ident
        character(len=:), allocatable :: code
        
        test_identifier_codegen = .true.
        print '(a)', "Testing identifier code generation..."
        
        ! Create identifier
        ident = create_identifier("x", 1, 1)
        
        ! Generate code
        code = generate_code(ident)
        
        ! Check generated code
        if (code /= "x") then
            print '(a)', "FAIL: Identifier code generation incorrect"
            print '(a)', "  Expected: 'x'"
            print '(a)', "  Got: '" // code // "'"
            test_identifier_codegen = .false.
        else
            print '(a)', "PASS: Identifier code generation"
        end if
    end function test_identifier_codegen

    logical function test_assignment_codegen()
        type(assignment_node) :: assign
        type(identifier_node) :: target
        type(literal_node) :: value
        character(len=:), allocatable :: code
        
        test_assignment_codegen = .true.
        print '(a)', "Testing assignment code generation..."
        
        ! Create assignment: x = 42
        target = create_identifier("x", 1, 1)
        value = create_literal("42", LITERAL_INTEGER, 1, 5)
        assign = create_assignment(target, value, 1, 1)
        
        ! Generate code
        code = generate_code(assign)
        
        ! Check generated code
        if (code /= "x = 42") then
            print '(a)', "FAIL: Assignment code generation incorrect"
            print '(a)', "  Expected: 'x = 42'"
            print '(a)', "  Got: '" // code // "'"
            test_assignment_codegen = .false.
        else
            print '(a)', "PASS: Assignment code generation"
        end if
    end function test_assignment_codegen

    logical function test_binary_op_codegen()
        type(binary_op_node) :: binop
        type(identifier_node) :: left, right
        character(len=:), allocatable :: code
        
        test_binary_op_codegen = .true.
        print '(a)', "Testing binary operation code generation..."
        
        ! Create binary operation: a + b
        left = create_identifier("a", 1, 1)
        right = create_identifier("b", 1, 5)
        binop = create_binary_op(left, right, "+", 1, 3)
        
        ! Generate code
        code = generate_code(binop)
        
        ! Check generated code
        if (code /= "a + b") then
            print '(a)', "FAIL: Binary operation code generation incorrect"
            print '(a)', "  Expected: 'a + b'"
            print '(a)', "  Got: '" // code // "'"
            test_binary_op_codegen = .false.
        else
            print '(a)', "PASS: Binary operation code generation"
        end if
    end function test_binary_op_codegen

end program test_codegen_basic