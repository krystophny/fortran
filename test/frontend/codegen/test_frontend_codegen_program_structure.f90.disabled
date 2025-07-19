program test_frontend_codegen_program_structure
    use ast_core
    use ast_factory
    use codegen_core
    use type_system_hm
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_program_with_implicit_none()
    call test_program_with_declarations()
    call test_program_with_contains()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_program_with_implicit_none()
        type(program_node) :: prog
        type(assignment_node) :: assign
        type(identifier_node) :: id_x
        type(literal_node) :: lit
        character(len=:), allocatable :: code

        test_count = test_count + 1

        ! Create simple program: x = 42
        prog%name = "test"

        id_x%name = "x"
        lit%value = "42"
        lit%literal_kind = LITERAL_INTEGER

        allocate (assign%target, source=id_x)
        allocate (assign%value, source=lit)

        allocate (prog%body(1))
        allocate (prog%body(1)%node, source=assign)

        ! Generate code
        code = generate_code(prog)

        ! Check for implicit none
        if (index(code, "implicit none") > 0) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Program generates implicit none"
        else
            write (*, '(A)') "FAIL: Program generates implicit none"
            write (*, '(A)') "Generated code:"
            write (*, '(A)') code
        end if
    end subroutine test_program_with_implicit_none

    subroutine test_program_with_declarations()
        type(program_node) :: prog
        type(assignment_node) :: assign1, assign2
        type(identifier_node) :: id_x, id_y
        type(literal_node) :: lit_int, lit_real
        character(len=:), allocatable :: code

        test_count = test_count + 1

        ! Create program with multiple assignments
        prog%name = "test"

        ! x = 42
        id_x%name = "x"
        lit_int%value = "42"
        lit_int%literal_kind = LITERAL_INTEGER
        allocate (assign1%target, source=id_x)
        allocate (assign1%value, source=lit_int)

        ! y = 3.14
        id_y%name = "y"
        lit_real%value = "3.14"
        lit_real%literal_kind = LITERAL_REAL
        allocate (assign2%target, source=id_y)
        allocate (assign2%value, source=lit_real)

        allocate (prog%body(2))
        allocate (prog%body(1)%node, source=assign1)
        allocate (prog%body(2)%node, source=assign2)

        ! Set inferred types manually for testing on the nodes in the program body
        select type (stmt => prog%body(1)%node)
        type is (assignment_node)
            select type (target => stmt%target)
            type is (identifier_node)
                allocate (target%inferred_type)
                target%inferred_type%kind = TINT
            end select
        end select

        select type (stmt => prog%body(2)%node)
        type is (assignment_node)
            select type (target => stmt%target)
            type is (identifier_node)
                allocate (target%inferred_type)
                target%inferred_type%kind = TREAL
            end select
        end select

        ! Generate code
        code = generate_code(prog)

        ! Check for variable declarations
        if (index(code, "integer :: x") > 0 .and. &
            index(code, "real(8) :: y") > 0) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Program generates variable declarations"
        else
            write (*, '(A)') "FAIL: Program generates variable declarations"
            write (*, '(A)') "Generated code:"
            write (*, '(A)') code
        end if
    end subroutine test_program_with_declarations

    subroutine test_program_with_contains()
        type(program_node) :: prog
        type(function_def_node) :: func
        type(identifier_node) :: id_f, id_x, id_ret
        type(assignment_node) :: assign, func_assign
        type(function_call_node) :: fcall
        type(literal_node) :: lit
        type(ast_node_wrapper) :: param_wrapper, body_wrapper
        character(len=:), allocatable :: code

        test_count = test_count + 1

        ! Create program with function
        prog%name = "test"

        ! Main: x = f(5)
        id_x%name = "x"
        fcall%name = "f"
        lit%value = "5"
        lit%literal_kind = LITERAL_INTEGER
        allocate (fcall%args(1))
        allocate (fcall%args(1)%node, source=lit)

        allocate (assign%target, source=id_x)
        allocate (assign%value, source=fcall)

        ! Function: function f(x)
        func%name = "f"
        id_f%name = "f"
        id_ret%name = "real"
        allocate (func%return_type, source=id_ret)

        ! Parameter
        id_x%name = "x"
        allocate (param_wrapper%node, source=id_x)
        allocate (func%params(1))
        func%params(1) = param_wrapper

        ! Function body: f = x
        allocate (func_assign%target, source=id_f)
        allocate (func_assign%value, source=id_x)
        allocate (body_wrapper%node, source=func_assign)
        allocate (func%body(1))
        func%body(1) = body_wrapper

        ! Build program
        allocate (prog%body(2))
        allocate (prog%body(1)%node, source=assign)
        allocate (prog%body(2)%node, source=func)

        ! Generate code
        code = generate_code(prog)

        ! Check for contains section
        if (index(code, "contains") > 0 .and. &
            index(code, "function f") > 0) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Program generates contains section"
        else
            write (*, '(A)') "FAIL: Program generates contains section"
            write (*, '(A)') "Generated code:"
            write (*, '(A)') code
        end if
    end subroutine test_program_with_contains

end program test_frontend_codegen_program_structure
