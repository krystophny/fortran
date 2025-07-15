program test_frontend_lexer_assignment
    use lexer_core, only: tokenize_core, token_t, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, TK_EOF
    implicit none
    
    type(token_t), allocatable :: tokens(:)
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    ! Test 1: Simple assignment "x = 1"
    call test_simple_assignment()
    
    if (all_tests_passed) then
        print *, "All lexer assignment tests passed!"
        stop 0
    else
        print *, "Some lexer assignment tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_simple_assignment()
        character(len=*), parameter :: input = "x = 1"
        
        print *, "Testing: '", input, "'"
        
        call tokenize_core(input, tokens)
        
        ! Should have 4 tokens: x, =, 1, EOF
        if (size(tokens) /= 4) then
            print *, "FAIL: Expected 4 tokens, got", size(tokens)
            all_tests_passed = .false.
            return
        end if
        
        ! Token 1: identifier "x"
        if (tokens(1)%kind /= TK_IDENTIFIER .or. tokens(1)%text /= "x") then
            print *, "FAIL: Token 1 should be identifier 'x'"
            all_tests_passed = .false.
        end if
        
        ! Token 2: operator "="
        if (tokens(2)%kind /= TK_OPERATOR .or. tokens(2)%text /= "=") then
            print *, "FAIL: Token 2 should be operator '='"
            all_tests_passed = .false.
        end if
        
        ! Token 3: number "1"
        if (tokens(3)%kind /= TK_NUMBER .or. tokens(3)%text /= "1") then
            print *, "FAIL: Token 3 should be number '1'"
            all_tests_passed = .false.
        end if
        
        ! Token 4: EOF
        if (tokens(4)%kind /= TK_EOF) then
            print *, "FAIL: Token 4 should be EOF"
            all_tests_passed = .false.
        end if
        
        if (all_tests_passed) print *, "PASS: Simple assignment tokenized correctly"
        
    end subroutine test_simple_assignment
    
end program test_frontend_lexer_assignment