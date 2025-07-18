program test_lexer_serialization_comprehensive
    use lexer_core
    use json_writer
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    ! Run comprehensive serialization tests
    if (.not. test_complex_expression_serialization()) all_passed = .false.
    if (.not. test_program_serialization()) all_passed = .false.
    if (.not. test_special_characters_serialization()) all_passed = .false.
    if (.not. test_large_input_serialization()) all_passed = .false.
    
    ! Report results
    if (all_passed) then
        print '(a)', "All comprehensive serialization tests passed"
        stop 0
    else
        print '(a)', "Some comprehensive serialization tests failed"
        stop 1
    end if

contains

    logical function test_complex_expression_serialization()
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: json_str
        
        test_complex_expression_serialization = .true.
        print '(a)', "Testing complex expression serialization..."
        
        ! Test complex mathematical expression
        call tokenize_core("result = (a + b) * c ** 2.5e-3", tokens)
        
        ! Convert to JSON
        json_str = json_write_tokens_to_string(tokens)
        
        ! Basic validation - should contain all expected tokens
        if (index(json_str, '"type": "identifier"') == 0) then
            print '(a)', "FAIL: JSON missing identifier tokens"
            test_complex_expression_serialization = .false.
            return
        end if
        
        if (index(json_str, '"type": "operator"') == 0) then
            print '(a)', "FAIL: JSON missing operator tokens"
            test_complex_expression_serialization = .false.
            return
        end if
        
        if (index(json_str, '"type": "number"') == 0) then
            print '(a)', "FAIL: JSON missing number tokens"
            test_complex_expression_serialization = .false.
            return
        end if
        
        if (index(json_str, '"text": "**"') == 0) then
            print '(a)', "FAIL: JSON missing power operator"
            test_complex_expression_serialization = .false.
            return
        end if
        
        if (index(json_str, '"text": "2.5e-3"') == 0) then
            print '(a)', "FAIL: JSON missing scientific notation"
            test_complex_expression_serialization = .false.
            return
        end if
        
        print '(a)', "PASS: Complex expression serialization"
    end function test_complex_expression_serialization

    logical function test_program_serialization()
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: json_str
        character(len=*), parameter :: test_file = "test_program_tokens.json"
        integer :: unit, iostat
        character(len=1000) :: file_content
        character(len=100) :: line
        
        test_program_serialization = .true.
        print '(a)', "Testing program structure serialization..."
        
        ! Test simple program structure
        call tokenize_core('program test; integer :: x; end program', tokens)
        
        ! Test file serialization
        call json_write_tokens_to_file(tokens, test_file)
        
        ! Read back and verify
        open(newunit=unit, file=test_file, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print '(a)', "FAIL: Could not read serialized file"
            test_program_serialization = .false.
            return
        end if
        
        file_content = ""
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            file_content = trim(file_content) // " " // trim(line)
        end do
        close(unit)
        
        ! Verify content
        if (index(file_content, '"text": "program"') == 0) then
            print '(a)', "FAIL: Program keyword not in serialized file"
            test_program_serialization = .false.
            return
        end if
        
        if (index(file_content, '"text": "::"') == 0) then
            print '(a)', "FAIL: Type declaration operator not in serialized file"
            test_program_serialization = .false.
            return
        end if
        
        ! Clean up
        open(newunit=unit, file=test_file, status='old')
        close(unit, status='delete')
        
        print '(a)', "PASS: Program structure serialization"
    end function test_program_serialization

    logical function test_special_characters_serialization()
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: json_str
        
        test_special_characters_serialization = .true.
        print '(a)', "Testing special characters serialization..."
        
        ! Test string with special characters
        call tokenize_core('"hello \"world\" \n"', tokens)
        
        json_str = json_write_tokens_to_string(tokens)
        
        ! Should contain the string token
        if (index(json_str, '"type": "string"') == 0) then
            print '(a)', "FAIL: String token not found in JSON"
            test_special_characters_serialization = .false.
            return
        end if
        
        ! Should properly escape the content
        if (index(json_str, '"tokens"') == 0) then
            print '(a)', "FAIL: JSON structure invalid"
            test_special_characters_serialization = .false.
            return
        end if
        
        print '(a)', "PASS: Special characters serialization"
    end function test_special_characters_serialization

    logical function test_large_input_serialization()
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: json_str
        character(len=:), allocatable :: large_input
        integer :: i
        
        test_large_input_serialization = .true.
        print '(a)', "Testing large input serialization..."
        
        ! Create a large input with many tokens
        large_input = ""
        do i = 1, 50
            if (i > 1) large_input = large_input // " + "
            large_input = large_input // "var" // char(48 + mod(i, 10))  ! var0, var1, etc.
        end do
        
        call tokenize_core(large_input, tokens)
        
        ! Should have many tokens (50 identifiers + 49 operators + 1 EOF = 100)
        if (size(tokens) /= 100) then
            print '(a,i0)', "FAIL: Expected 100 tokens, got: ", size(tokens)
            test_large_input_serialization = .false.
            return
        end if
        
        ! Test serialization
        json_str = json_write_tokens_to_string(tokens)
        
        ! Should contain appropriate number of tokens in JSON
        if (len(json_str) < 1000) then
            print '(a)', "FAIL: JSON output seems too short for large input"
            test_large_input_serialization = .false.
            return
        end if
        
        if (index(json_str, '"tokens"') == 0) then
            print '(a)', "FAIL: JSON structure invalid for large input"
            test_large_input_serialization = .false.
            return
        end if
        
        print '(a)', "PASS: Large input serialization"
    end function test_large_input_serialization

end program test_lexer_serialization_comprehensive