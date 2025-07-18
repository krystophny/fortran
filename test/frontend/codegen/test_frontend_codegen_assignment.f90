program test_frontend_codegen_assignment
    use ast_core
    use codegen_core, only: generate_code_polymorphic
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    ! Test 1: Simple assignment code generation
    call test_simple_assignment_codegen()
    
    if (all_tests_passed) then
        print *, "All codegen assignment tests passed!"
        stop 0
    else
        print *, "Some codegen assignment tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_simple_assignment_codegen()
        type(assignment_node) :: assign
        class(ast_node), allocatable :: target, value
        character(len=:), allocatable :: code
        
        print *, "Testing: assignment code generation for 'x = 1'"
        
        ! Create AST nodes
        target = create_identifier("x", 1, 1)
        value = create_literal("1", LITERAL_INTEGER, 1, 5)
        assign = create_assignment(target, value, 1, 1)
        
        ! Generate code
        code = generate_code_polymorphic(assign)
        
        ! Check result
        if (code == "x = 1") then
            print *, "PASS: Generated code is correct: '", code, "'"
        else
            print *, "FAIL: Expected 'x = 1', got '", code, "'"
            all_tests_passed = .false.
        end if
        
    end subroutine test_simple_assignment_codegen
    
end program test_frontend_codegen_assignment