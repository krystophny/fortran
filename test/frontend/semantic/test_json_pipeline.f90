program test_json_pipeline
    use frontend
    use semantic_analyzer
    use type_system_hm
    use ast_core
    use json_writer
    use json_reader
    use temp_utils, only: temp_dir_manager
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0
    type(temp_dir_manager) :: temp_mgr

    write (*, '(a)') '=== Testing JSON Pipeline with Semantic Analysis ==='
    write (*, '(a)') ''

    call test_ast_to_json_roundtrip()
    call test_semantic_analysis_preserves_structure()
    call test_type_annotations_in_json()
    call test_from_ast_compilation()

    write (*, '(a)') ''
    write(*, '(a,i0,a,i0,a)') 'JSON pipeline tests: ', pass_count, '/', test_count, ' passed'

    if (pass_count /= test_count) then
        error stop 'Some tests failed!'
    end if

contains

    subroutine test_ast_to_json_roundtrip()
        type(program_node) :: prog
        type(assignment_node) :: assign
        type(identifier_node) :: var
        type(literal_node) :: val
        type(ast_node_wrapper) :: body_wrapper
        character(len=:), allocatable :: json_str, ast_file
        character(len=256) :: error_msg
        class(ast_node), allocatable :: loaded_ast

        write (*, '(a)') 'Test 1: AST to JSON roundtrip'
        test_count = test_count + 1

        ! Create simple AST: x = 42
        var = create_identifier("x", 1, 1)
        val = create_literal(LITERAL_INTEGER, "42", 1, 5)
        assign = create_assignment(var, val, 1, 1)
        allocate (body_wrapper%node, source=assign)

        prog = create_program("test", [body_wrapper], 1, 1)

        ! Create temp directory
        call temp_mgr%create('test_json_roundtrip')
        ast_file = temp_mgr%get_file_path('test_ast.json')

        ! Write AST to JSON
        call json_write_ast_to_file(prog, ast_file)

        ! Read AST back from JSON
        call json_read_ast_from_file(ast_file, loaded_ast, error_msg)

        if (len_trim(error_msg) == 0) then
            ! Check if it's still a program node
            select type (loaded => loaded_ast)
            type is (program_node)
                if (loaded%name == "test") then
                    pass_count = pass_count + 1
                    write (*, '(a)') '  ✓ PASS: AST roundtrip through JSON successful'
                else
                    write (*, '(a)') '  ✗ FAIL: Program name not preserved'
                end if
            class default
                write (*, '(a)') '  ✗ FAIL: Wrong AST node type after loading'
            end select
        else
            write (*, '(a)') '  ✗ FAIL: Error loading AST from JSON'
            write (*, '(a)') '    Error: '//trim(error_msg)
        end if

    end subroutine test_ast_to_json_roundtrip

    subroutine test_semantic_analysis_preserves_structure()
        type(semantic_context_t) :: ctx
        type(program_node) :: prog
        type(assignment_node) :: assign1, assign2
        type(identifier_node) :: x_var, y_var
        type(literal_node) :: int_val, real_val
        type(ast_node_wrapper) :: body_wrapper1, body_wrapper2
        character(len=:), allocatable :: ast_file

        write (*, '(a)') 'Test 2: Semantic analysis preserves AST structure'
        test_count = test_count + 1

        ! Create program with multiple assignments
        x_var = create_identifier("x", 1, 1)
        int_val = create_literal(LITERAL_INTEGER, "42", 1, 5)
        assign1 = create_assignment(x_var, int_val, 1, 1)
        allocate (body_wrapper1%node, source=assign1)

        y_var = create_identifier("y", 2, 1)
        real_val = create_literal(LITERAL_REAL, "3.14", 2, 5)
        assign2 = create_assignment(y_var, real_val, 2, 1)
        allocate (body_wrapper2%node, source=assign2)

        prog = create_program("test", [body_wrapper1, body_wrapper2], 1, 1)

        ! Run semantic analysis
        ctx = create_semantic_context()
        call analyze_program(ctx, prog)

        ! Check structure is preserved
        if (allocated(prog%body)) then
            if (size(prog%body) == 2) then
                pass_count = pass_count + 1
            write (*, '(a)') '  ✓ PASS: AST structure preserved after semantic analysis'
            else
                write (*, '(a)') '  ✗ FAIL: Body size changed'
            end if
        else
            write (*, '(a)') '  ✗ FAIL: Body deallocated'
        end if

    end subroutine test_semantic_analysis_preserves_structure

    subroutine test_type_annotations_in_json()
        type(semantic_context_t) :: ctx
        type(assignment_node) :: assign
        type(identifier_node) :: var
        type(literal_node) :: val
        type(mono_type_t) :: inferred_type

        write (*, '(a)') 'Test 3: Type annotations stored in AST'
        test_count = test_count + 1

        ! Create assignment: pi = 3.14159
        var = create_identifier("pi", 1, 1)
        val = create_literal(LITERAL_REAL, "3.14159", 1, 5)
        assign = create_assignment(var, val, 1, 1)

        ! Run semantic analysis
        ctx = create_semantic_context()
        call analyze_program(ctx, assign)

        ! Check if types were stored
        if (allocated(assign%inferred_type)) then
            if (assign%inferred_type%kind == TREAL) then
                pass_count = pass_count + 1
                write (*, '(a)') '  ✓ PASS: Type annotations correctly stored'
            else
                write (*, '(a)') '  ✗ FAIL: Wrong type inferred'
            end if
        else
            write (*, '(a)') '  ✗ FAIL: No type annotation stored'
        end if

    end subroutine test_type_annotations_in_json

    subroutine test_from_ast_compilation()
        type(compilation_options_t) :: options
        character(len=256) :: error_msg
        character(len=:), allocatable :: ast_file, source_file
        integer :: unit

        write (*, '(a)') 'Test 4: Compilation from AST JSON'
        test_count = test_count + 1

        ! Create temp directory
        call temp_mgr%create('test_from_ast')
        source_file = temp_mgr%get_file_path('test.f')
        ast_file = temp_mgr%get_file_path('test_ast.json')

        ! Create a simple source file
        open (newunit=unit, file=source_file, status='replace')
        write (unit, '(a)') 'x = 42'
        write (unit, '(a)') 'y = x + 1'
        write (unit, '(a)') 'print *, y'
        close (unit)

        ! First compile with AST output
        options%debug_ast = .true.
        options%output_file = ast_file

        ! This should create the AST JSON file
        call compile_source(source_file, options, error_msg)

        if (len_trim(error_msg) == 0) then
            ! Now try to compile from the AST JSON
            options%debug_ast = .false.
            options%output_file = temp_mgr%get_file_path('output.f90')

            call compile_from_ast_json(ast_file, options, error_msg)

            if (len_trim(error_msg) == 0) then
                pass_count = pass_count + 1
                write (*, '(a)') '  ✓ PASS: Compilation from AST JSON successful'
            else
                write (*, '(a)') '  ✗ FAIL: Compilation from AST failed'
                write (*, '(a)') '    Error: '//trim(error_msg)
            end if
        else
            write (*, '(a)') '  ✗ FAIL: Initial compilation failed'
            write (*, '(a)') '    Error: '//trim(error_msg)
        end if

    end subroutine test_from_ast_compilation

end program test_json_pipeline
