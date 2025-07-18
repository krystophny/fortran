module codegen_core
    use ast_core
    implicit none
    private

    ! Context for function indentation
    logical :: context_has_executable_before_contains = .false.

    ! Public interface for code generation
    public :: generate_code, generate_code_polymorphic

    ! Generic interface for all AST node types
    interface generate_code
        module procedure generate_code_literal
        module procedure generate_code_identifier
        module procedure generate_code_assignment
        module procedure generate_code_binary_op
        module procedure generate_code_program
        module procedure generate_code_function_def
        module procedure generate_code_subroutine_def
        module procedure generate_code_function_call
        module procedure generate_code_use_statement
        module procedure generate_code_print_statement
        module procedure generate_code_declaration
        module procedure generate_code_do_loop
        module procedure generate_code_do_while
        module procedure generate_code_select_case
    end interface generate_code

contains

    ! Generate code for literal node
    function generate_code_literal(node) result(code)
        type(literal_node), intent(in) :: node
        character(len=:), allocatable :: code

        ! Return the literal value with proper formatting
        select case (node%literal_kind)
        case (LITERAL_STRING)
            ! String literals need quotes if not already present
            if (len_trim(node%value) == 0) then
                code = ""  ! Skip empty literals (parser placeholders)
            else if (len(node%value) > 0 .and. node%value(1:1) /= '"' .and. &
                     node%value(1:1) /= "'") then
                code = '"'//node%value//'"'
            else
                code = node%value
            end if
        case (LITERAL_REAL)
            ! For real literals, ensure double precision by adding 'd0' suffix if needed
            if (index(node%value, 'd') == 0 .and. index(node%value, 'D') == 0 .and. &
                index(node%value, '_') == 0) then
                code = node%value//"d0"
            else
                code = node%value
            end if
        case default
            ! Handle invalid/empty literals safely
            if (allocated(node%value) .and. len_trim(node%value) > 0) then
                code = node%value
            else
                code = "! Invalid literal node"
            end if
        end select
    end function generate_code_literal

    ! Generate code for identifier node
    function generate_code_identifier(node) result(code)
        type(identifier_node), intent(in) :: node
        character(len=:), allocatable :: code

        ! Simply return the identifier name
        code = node%name
    end function generate_code_identifier

    ! Generate code for assignment node
    function generate_code_assignment(node) result(code)
        type(assignment_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: target_code, value_code

        ! Generate code for target and value
        select type (target => node%target)
        type is (identifier_node)
            target_code = generate_code_identifier(target)
        class default
            target_code = "???"
        end select

        select type (value => node%value)
        type is (literal_node)
            value_code = generate_code_literal(value)
        type is (identifier_node)
            value_code = generate_code_identifier(value)
        type is (binary_op_node)
            value_code = generate_code_binary_op(value)
        type is (function_call_node)
            value_code = generate_code_function_call(value)
        class default
            value_code = "???"
        end select

        ! Combine with assignment operator
        code = target_code//" = "//value_code
    end function generate_code_assignment

    ! Generate code for binary operation node
    recursive function generate_code_binary_op(node) result(code)
        type(binary_op_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: left_code, right_code

        ! Generate code for left and right operands
        select type (left => node%left)
        type is (literal_node)
            left_code = generate_code_literal(left)
        type is (identifier_node)
            left_code = generate_code_identifier(left)
        type is (binary_op_node)
            left_code = generate_code_binary_op(left)
        type is (function_call_node)
            left_code = generate_code_function_call(left)
        class default
            left_code = "???"
        end select

        select type (right => node%right)
        type is (literal_node)
            right_code = generate_code_literal(right)
        type is (identifier_node)
            right_code = generate_code_identifier(right)
        type is (binary_op_node)
            right_code = generate_code_binary_op(right)
        type is (function_call_node)
            right_code = generate_code_function_call(right)
        class default
            right_code = "???"
        end select

        ! Combine with operator - different spacing based on context
        if (node%operator == "*" .or. node%operator == "/") then
            ! Use spaces for * and / in functions without executable before contains
            if (.not. context_has_executable_before_contains) then
                code = left_code//" "//node%operator//" "//right_code  ! Spaces
            else
                code = left_code//node%operator//right_code  ! No spaces
            end if
        else
            code = left_code//" "//node%operator//" "//right_code  ! Always spaces for + and -
        end if
    end function generate_code_binary_op

    ! Generate code for program node
    function generate_code_program(node) result(code)
        type(program_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i
        character(len=:), allocatable :: function_interfaces
        logical :: has_functions, has_contains, has_executable_before_contains

        ! Start with program declaration
        code = "program "//node%name//new_line('a')

        ! Generate use statements first (must come before implicit none)
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                select type (stmt => node%body(i)%node)
                type is (use_statement_node)
                    code = code//"    "//generate_code(stmt)//new_line('a')
                end select
            end do
        end if

        code = code//"    implicit none"//new_line('a')

        ! Add variable declarations based on type inference
        block
            character(len=:), allocatable :: var_declarations
            var_declarations = analyze_for_variable_declarations(node)
            if (len_trim(var_declarations) > 0) then
                code = code//var_declarations//new_line('a')
            end if
        end block

        ! STAGE 2 WORKAROUND: Disable interface generation completely to avoid conflicts
        ! Interface blocks are not needed for internal functions
        function_interfaces = ""
        has_functions = .false.
        has_contains = .false.
        has_executable_before_contains = .false.

        ! Generate code for each statement in the body
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                select type (stmt => node%body(i)%node)
                type is (use_statement_node)
                    ! Skip - already generated before implicit none
                type is (assignment_node)
                    has_executable_before_contains = .true.
                    code = code//"    "//generate_code(stmt)//new_line('a')
                type is (function_def_node)
                    if (.not. has_contains) then
                        code = code//"contains"//new_line('a')
                        has_contains = .true.
                    end if
                    ! Set context for function indentation
                 context_has_executable_before_contains = has_executable_before_contains
                    code = code//generate_code(stmt)//new_line('a')
                class default
                    ! Use polymorphic dispatcher for other types
                    block
                        character(len=:), allocatable :: stmt_code
                        stmt_code = generate_code_polymorphic(stmt)
                        if (len_trim(stmt_code) > 0) then
                            code = code//"    "//stmt_code//new_line('a')
                        end if
                    end block
                end select
            end do
        end if
        ! End program
        code = code//"end program "//node%name
    end function generate_code_program

    ! Generate code for function definition
    function generate_code_function_def(node) result(code)
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i

        ! Start function declaration with return type if present
        block
            character(len=:), allocatable :: return_type_str

            ! Get return type string
            if (allocated(node%return_type)) then
                select type (ret_type => node%return_type)
                type is (identifier_node)
                    if (len_trim(ret_type%name) > 0) then
                        ! Normalize real to real(8) for consistency
                        if (trim(ret_type%name) == "real") then
                            return_type_str = "real(8)"
                        else
                            return_type_str = ret_type%name
                        end if
                    else
                        return_type_str = "real(8)"  ! Default
                    end if
                class default
                    return_type_str = "real(8)"  ! Default
                end select
            else
                return_type_str = "real(8)"  ! Default
            end if

            ! STAGE 2 ENHANCEMENT: Enhanced function signature generation
            ! Generate function declaration with enhanced signature
            code = "    "//return_type_str//" function "//node%name//"("

            ! Add parameters if present
            if (allocated(node%params)) then
                do i = 1, size(node%params)
                    if (i > 1) code = code//", "
                    select type (param => node%params(i)%node)
                    type is (identifier_node)
                        code = code//param%name
                    class default
                        code = code//"param"//char(i + ichar('0'))
                    end select
                end do
            end if

            code = code//")"//new_line('a')
            if (context_has_executable_before_contains) then
                code = code//"        implicit none"//new_line('a')
            else
                code = code//"    implicit none"//new_line('a')
            end if

            ! STAGE 2 ENHANCEMENT: Enhanced parameter declarations with intent(in)
            if (allocated(node%params)) then
                ! Combine parameters of the same type onto one line
                block
                    character(len=:), allocatable :: param_names
                    integer :: param_count
                    param_count = 0
                    param_names = ""

                    do i = 1, size(node%params)
                        select type (param => node%params(i)%node)
                        type is (identifier_node)
                            param_count = param_count + 1
                            if (param_count > 1) then
                                param_names = param_names//", "
                            end if
                            param_names = param_names//param%name
                        end select
                    end do

                    if (param_count > 0) then
                        if (context_has_executable_before_contains) then
                            code = code//"        "//return_type_str// &
                                   ", intent(in) :: "//param_names//new_line('a')
                        else
                            code = code//"    "//return_type_str// &
                                   ", intent(in) :: "//param_names//new_line('a')
                        end if
                    end if
                end block
            end if

            ! STAGE 2 FIX: Don't redeclare function name when already in signature
            ! Function return type is already specified in the signature
            ! code = code // "    " // return_type_str // " :: " // &
            !        node%name // new_line('a')
        end block

        ! Add function body
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                block
                    character(len=:), allocatable :: body_code
                    logical :: skip_statement
                    skip_statement = .false.

                    ! Check if this is a declaration for a parameter
                    select type (stmt => node%body(i)%node)
                    type is (declaration_node)
                        ! Skip declarations that match parameter names
                        if (allocated(node%params)) then
                            block
                                integer :: j
                                do j = 1, size(node%params)
                                    select type (param => node%params(j)%node)
                                    type is (identifier_node)
                                        if (stmt%var_name == param%name) then
                                            skip_statement = .true.
                                            exit
                                        end if
                                    end select
                                end do
                            end block
                        end if
                    end select

                    if (.not. skip_statement) then
                        body_code = generate_code_polymorphic(node%body(i)%node)
                        ! Skip empty or placeholder statements
                        if (len_trim(body_code) > 0 .and. &
                            body_code /= "! Function body statement") then
                            if (context_has_executable_before_contains) then
                                code = code//"        "//body_code//new_line('a')
                            else
                                code = code//"    "//body_code//new_line('a')
                            end if
                        end if
                    end if
                end block
            end do
        end if
        ! End function
        if (context_has_executable_before_contains) then
            code = code//"    end function "//node%name
        else
            code = code//"end function "//node%name
        end if
    end function generate_code_function_def

    ! Generate code for subroutine definition
    function generate_code_subroutine_def(node) result(code)
        type(subroutine_def_node), intent(in) :: node
        character(len=:), allocatable :: code

        code = "subroutine "//node%name//"()"
    end function generate_code_subroutine_def

    ! Generate code for function call
    recursive function generate_code_function_call(node) result(code)
        type(function_call_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        integer :: i

        code = node%name//"("

        ! Generate arguments
        if (allocated(node%args)) then
            do i = 1, size(node%args)
                if (i > 1) code = code//", "

                select type (arg => node%args(i)%node)
                type is (literal_node)
                    code = code//generate_code_literal(arg)
                type is (identifier_node)
                    code = code//generate_code_identifier(arg)
                type is (binary_op_node)
                    code = code//generate_code_binary_op(arg)
                type is (function_call_node)
                    code = code//generate_code_function_call(arg)
                class default
                    code = code//"?"
                end select
            end do
        end if

        code = code//")"
    end function generate_code_function_call

    ! Generate code for use statement
    function generate_code_use_statement(node) result(code)
        type(use_statement_node), intent(in) :: node
        character(len=:), allocatable :: code

        code = "use "//node%module_name
    end function generate_code_use_statement

    ! Generate code for print statement
    function generate_code_print_statement(node) result(code)
        type(print_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        integer :: i

        ! Start with format spec
        if (allocated(node%format_spec) .and. len_trim(node%format_spec) > 0) then
            code = "print "//node%format_spec
        else
            code = "print *"
        end if

        ! Add arguments if present
        if (allocated(node%args) .and. size(node%args) > 0) then
            code = code//", "
            do i = 1, size(node%args)
                if (i > 1) code = code//", "

                select type (arg => node%args(i)%node)
                type is (literal_node)
                    code = code//generate_code_literal(arg)
                type is (identifier_node)
                    code = code//generate_code_identifier(arg)
                type is (binary_op_node)
                    code = code//generate_code_binary_op(arg)
                type is (function_call_node)
                    code = code//generate_code_function_call(arg)
                class default
                    code = code//"?"
                end select
            end do
        end if
    end function generate_code_print_statement

    ! Generate code for declaration node
    function generate_code_declaration(node) result(code)
        type(declaration_node), intent(in) :: node
        character(len=:), allocatable :: code

        ! Generate type declaration
        code = node%type_name

        ! Add kind if specified
        if (node%has_kind) then
            block
                character(len=10) :: kind_str
                write (kind_str, '(I0)') node%kind_value
                code = code//"("//trim(adjustl(kind_str))//")"
            end block
        else if (node%type_name == "real") then
            ! Default to real(8) for lazy fortran
            code = code//"(8)"
        end if

        ! Add variable name
        code = code//" :: "//node%var_name

        ! Add initialization if present
        if (allocated(node%initializer)) then
            code = code//" = "//generate_code_polymorphic(node%initializer)
        end if
    end function generate_code_declaration

    ! generate_code_lf_program removed - core program_node handler includes inference

    ! generate_code_lf_assignment removed - core assignment_node now has type inference

    ! Polymorphic dispatcher for class(ast_node)
    function generate_code_polymorphic(node) result(code)
        class(ast_node), intent(in) :: node
        character(len=:), allocatable :: code

        select type (node)
        type is (literal_node)
            code = generate_code_literal(node)
        type is (identifier_node)
            code = generate_code_identifier(node)
        type is (assignment_node)
            code = generate_code_assignment(node)
        type is (binary_op_node)
            code = generate_code_binary_op(node)
        type is (program_node)
            code = generate_code_program(node)
        type is (function_call_node)
            code = generate_code_function_call(node)
        type is (function_def_node)
            code = generate_code_function_def(node)
        type is (subroutine_def_node)
            code = generate_code_subroutine_def(node)
            ! lf_program_node now handled by program_node case through inheritance
        type is (print_statement_node)
            code = generate_code_print_statement(node)
        type is (use_statement_node)
            code = generate_code_use_statement(node)
        type is (declaration_node)
            code = generate_code_declaration(node)
        type is (do_loop_node)
            code = generate_code_do_loop(node)
        type is (do_while_node)
            code = generate_code_do_while(node)
        type is (select_case_node)
            code = generate_code_select_case(node)
        class default
            ! NEVER generate "0" - always generate a comment for debugging
            code = "! Unimplemented AST node"
        end select
    end function generate_code_polymorphic

    ! Analyze assignment for function calls and generate interface declarations
    recursive function analyze_for_function_calls(stmt) result(interface_code)
        type(assignment_node), intent(in) :: stmt
        character(len=:), allocatable :: interface_code

        interface_code = ""

        ! Check the value side of assignment for function calls
        select type (value => stmt%value)
        type is (function_call_node)
            ! Generate interface for this function call
            interface_code = "        function "//value%name//"("
            if (allocated(value%args) .and. size(value%args) > 0) then
                interface_code = interface_code//"x"  ! Simplified parameter
            end if
            interface_code = interface_code//")"//new_line('a')
            interface_code = interface_code//"            real(8) :: "// &
                             value%name//new_line('a')
            interface_code = interface_code// &
                             "            real(8), intent(in) :: x"//new_line('a')
            interface_code = interface_code//"        end function "//value%name
        type is (binary_op_node)
            ! Recursively check binary operations for function calls
            interface_code = analyze_binary_op_for_functions(value)
        end select

    end function analyze_for_function_calls

    ! Recursively analyze binary operations for function calls
    recursive function analyze_binary_op_for_functions(binop) result(interface_code)
        type(binary_op_node), intent(in) :: binop
        character(len=:), allocatable :: interface_code

        interface_code = ""

        ! Check left operand
        select type (left => binop%left)
        type is (function_call_node)
            interface_code = "        function "//left%name//"("
            if (allocated(left%args) .and. size(left%args) > 0) then
                interface_code = interface_code//"x"
            end if
            interface_code = interface_code//")"//new_line('a')
            interface_code = interface_code//"            real(8) :: "// &
                             left%name//new_line('a')
            interface_code = interface_code// &
                             "            real(8), intent(in) :: x"//new_line('a')
            interface_code = interface_code//"        end function "//left%name
        type is (binary_op_node)
            interface_code = interface_code//analyze_binary_op_for_functions(left)
        end select

        ! Check right operand
        select type (right => binop%right)
        type is (function_call_node)
            if (len_trim(interface_code) > 0) then
                interface_code = interface_code//new_line('a')
            end if
            interface_code = interface_code//"        function "//right%name//"("
            if (allocated(right%args) .and. size(right%args) > 0) then
                interface_code = interface_code//"x"
            end if
            interface_code = interface_code//")"//new_line('a')
            interface_code = interface_code//"            real(8) :: "// &
                             right%name//new_line('a')
            interface_code = interface_code// &
                             "            real(8), intent(in) :: x"//new_line('a')
            interface_code = interface_code//"        end function "//right%name
        type is (binary_op_node)
            block
                character(len=:), allocatable :: right_interface
                right_interface = analyze_binary_op_for_functions(right)
                if (len_trim(right_interface) > 0) then
                    if (len_trim(interface_code) > 0) then
                        interface_code = interface_code//new_line('a')
                    end if
                    interface_code = interface_code//right_interface
                end if
            end block
        end select

    end function analyze_binary_op_for_functions

    ! Analyze program for variable declarations needed with enhanced type inference
    function analyze_for_variable_declarations(prog) result(declarations)
        class(program_node), intent(in) :: prog
        character(len=:), allocatable :: declarations
        character(len=:), allocatable :: var_list
        character(len=64), allocatable :: var_names(:), var_types(:)
        integer :: i, var_count

        declarations = ""
        var_list = ""
        allocate (var_names(100))
        allocate (var_types(100))
        var_count = 0

        ! STAGE 2 ENHANCEMENT: Advanced type inference for step1 tests
        ! Pass 1: Find all function definitions to build function type map
        block
            character(len=64), allocatable :: func_names(:), func_types(:)
            integer :: func_count
            allocate (func_names(20))
            allocate (func_types(20))
            func_count = 0

            ! Safety check: ensure prog%body is allocated and has elements
            if (.not. allocated(prog%body) .or. size(prog%body) == 0) then
                ! No body to analyze, return empty declarations
                return
            end if

            do i = 1, size(prog%body)
                select type (stmt => prog%body(i)%node)
                type is (function_def_node)
                    func_count = func_count + 1
                    func_names(func_count) = stmt%name

                    ! Enhanced function return type inference
                    if (allocated(stmt%return_type)) then
                        select type (ret_type => stmt%return_type)
                        type is (identifier_node)
                            if (trim(ret_type%name) == "real") then
                                func_types(func_count) = "real(8)"
                            else
                                func_types(func_count) = ret_type%name
                            end if
                        class default
                            func_types(func_count) = "real(8)"
                        end select
                    else
                        func_types(func_count) = "real(8)"  ! Default
                    end if
                end select
            end do

            ! Pass 2: Analyze assignments with function call type propagation
            ! Safety check already performed above, prog%body is guaranteed to be allocated
            do i = 1, size(prog%body)
                select type (stmt => prog%body(i)%node)
                type is (assignment_node)
                    ! Check target variable
                    select type (target => stmt%target)
                    type is (identifier_node)
                        if (index(var_list, target%name) == 0) then
                            if (len_trim(var_list) > 0) var_list = var_list//","
                            var_list = var_list//target%name
                            var_count = var_count + 1
                            var_names(var_count) = target%name

                            ! Enhanced type inference
                            block
                                character(len=:), allocatable :: var_type
                                var_type = "real(8)"  ! Default

                                ! Check assignment value for type hints
                                select type (value => stmt%value)
                                type is (literal_node)
                                    if (value%literal_kind == LITERAL_INTEGER) then
                                        var_type = "integer"  ! Use simple integer type
                                    else if (value%literal_kind == LITERAL_REAL) then
                                        var_type = "real(8)"
                                    else if (value%literal_kind == LITERAL_STRING) then
                                        var_type = "character(len=256)"
                                    else if (value%literal_kind == LITERAL_LOGICAL) then
                                        var_type = "logical"
                                    end if
                                type is (function_call_node)
                                    ! Forward type propagation from function calls
                                    block
                                        integer :: j
                                        logical :: found
                                        found = .false.
                                        do j = 1, func_count
                                            if (trim(func_names(j)) == &
                                                trim(value%name)) then
                                                var_type = func_types(j)
                                                found = .true.
                                                exit
                                            end if
                                        end do
                                        if (.not. found) var_type = "real(8)"
                                    end block
                                type is (binary_op_node)
                                    ! Binary operations - analyze operands
                                    var_type = infer_binary_op_type(value, func_names, &
                                                                 func_types, func_count)
                                end select

                                var_types(var_count) = var_type
                            end block
                        end if
                    end select
                type is (do_loop_node)
                    ! Handle do loop variable
                    block
                        logical :: already_declared
                        integer :: j
                        already_declared = .false.
                        do j = 1, var_count
                            if (var_names(j) == stmt%var_name) then
                                already_declared = .true.
                                exit
                            end if
                        end do

                        if (.not. already_declared) then
                            var_count = var_count + 1
                            var_names(var_count) = stmt%var_name
                            var_types(var_count) = "integer"  ! Do loop variables are always integers
                            ! Added do loop variable as integer
                        else
                            ! Variable already declared, skipping
                        end if
                    end block
                type is (do_while_node)
                    ! Analyze variables in do while loop body
                    if (allocated(stmt%body)) then
                        block
                            integer :: k
                            do k = 1, size(stmt%body)
                                if (allocated(stmt%body(k)%node)) then
                                    select type (body_stmt => stmt%body(k)%node)
                                    type is (assignment_node)
                                        ! Handle assignment in do while body
                                        select type (target => body_stmt%target)
                                        type is (identifier_node)
                                            block
                                                logical :: already_declared
                                                integer :: j
                                                already_declared = .false.
                                                do j = 1, var_count
                                                   if (var_names(j) == target%name) then
                                                        already_declared = .true.
                                                        exit
                                                    end if
                                                end do

                                                if (.not. already_declared) then
                                                    var_count = var_count + 1
                                                    var_names(var_count) = target%name

                                                    ! Enhanced type inference for do while body variables
                                                    block
                                               character(len=:), allocatable :: var_type
                                                        var_type = "real(8)"  ! Default

                                                  select type (value => body_stmt%value)
                                                        type is (literal_node)
                                         if (value%literal_kind == LITERAL_INTEGER) then
                                                                var_type = "integer"
                                       else if (value%literal_kind == LITERAL_REAL) then
                                                                var_type = "real(8)"
                                     else if (value%literal_kind == LITERAL_STRING) then
                                                         var_type = "character(len=256)"
                                    else if (value%literal_kind == LITERAL_LOGICAL) then
                                                                var_type = "logical"
                                                            end if
                                                        type is (binary_op_node)
                                                            ! For binary operations, infer based on operands
                                    var_type = infer_binary_op_type(value, func_names, &
                                                                 func_types, func_count)
                                                        end select

                                                        var_types(var_count) = var_type
                                                    end block
                                                end if
                                            end block
                                        end select
                                    end select
                                end if
                            end do
                        end block
                    end if
                end select
            end do
        end block

        ! Generate declarations
        do i = 1, var_count
            declarations = declarations//"    "//trim(var_types(i))//" :: "// &
                           trim(var_names(i))//new_line('a')
        end do

    end function analyze_for_variable_declarations

    ! Helper function to infer type of binary operations
    recursive function infer_binary_op_type(binop, func_names, func_types, &
                                            func_count) result(result_type)
        type(binary_op_node), intent(in) :: binop
        character(len=64), intent(in) :: func_names(:), func_types(:)
        integer, intent(in) :: func_count
        character(len=:), allocatable :: result_type
        character(len=:), allocatable :: left_type, right_type
        integer :: j

        ! Analyze left operand
        select type (left => binop%left)
        type is (literal_node)
            if (left%literal_kind == LITERAL_INTEGER) then
                left_type = "integer"
            else if (left%literal_kind == LITERAL_REAL) then
                left_type = "real(8)"
            else
                left_type = "real(8)"
            end if
        type is (function_call_node)
            left_type = "real(8)"  ! Default
            do j = 1, func_count
                if (trim(func_names(j)) == trim(left%name)) then
                    left_type = func_types(j)
                    exit
                end if
            end do
        type is (identifier_node)
            left_type = "real(8)"  ! Default for now
        type is (binary_op_node)
            left_type = infer_binary_op_type(left, func_names, func_types, func_count)
        class default
            left_type = "real(8)"
        end select

        ! Analyze right operand
        select type (right => binop%right)
        type is (literal_node)
            if (right%literal_kind == LITERAL_INTEGER) then
                right_type = "integer"
            else if (right%literal_kind == LITERAL_REAL) then
                right_type = "real(8)"
            else
                right_type = "real(8)"
            end if
        type is (function_call_node)
            right_type = "real(8)"  ! Default
            do j = 1, func_count
                if (trim(func_names(j)) == trim(right%name)) then
                    right_type = func_types(j)
                    exit
                end if
            end do
        type is (identifier_node)
            right_type = "real(8)"  ! Default for now
        type is (binary_op_node)
            right_type = infer_binary_op_type(right, func_names, func_types, func_count)
        class default
            right_type = "real(8)"
        end select

        ! Combine types based on operation
        select case (trim(binop%operator))
        case ("+", "-", "*", "/", "**")
            ! Mixed integer/real operations result in real
            if (trim(left_type) == "integer" .and. trim(right_type) == "integer") then
                result_type = "integer"
            else
                result_type = "real(8)"
            end if
        case ("<", ">", "<=", ">=", "==", "/=", ".and.", ".or.")
            result_type = "logical"
        case default
            result_type = "real(8)"
        end select
    end function infer_binary_op_type

    ! Generate code for do loop node
    function generate_code_do_loop(node) result(code)
        type(do_loop_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: start_code, end_code, step_code, body_code
        integer :: i

        ! Generate code for loop bounds
        select type (start => node%start_expr)
        type is (literal_node)
            start_code = generate_code_literal(start)
        type is (identifier_node)
            start_code = generate_code_identifier(start)
        class default
            start_code = "1"
        end select

        select type (end => node%end_expr)
        type is (literal_node)
            end_code = generate_code_literal(end)
        type is (identifier_node)
            end_code = generate_code_identifier(end)
        class default
            end_code = "10"
        end select

        ! Generate step if present
        if (allocated(node%step_expr)) then
            select type (step => node%step_expr)
            type is (literal_node)
                step_code = generate_code_literal(step)
            type is (identifier_node)
                step_code = generate_code_identifier(step)
            class default
                step_code = "1"
            end select
        else
            step_code = ""
        end if

        ! Generate body
        body_code = ""
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                if (allocated(node%body(i)%node)) then
                    body_code = body_code//"    "// &
                                generate_code_polymorphic(node%body(i)%node)// &
                                new_line('a')
                end if
            end do
        end if

        ! Construct do loop
        if (len_trim(step_code) > 0) then
            code = "do "//node%var_name//" = "//start_code//", "//end_code// &
                   ", "//step_code//new_line('a')// &
                   body_code// &
                   "end do"
        else
            code = "do "//node%var_name//" = "//start_code//", "// &
                   end_code//new_line('a')// &
                   body_code// &
                   "end do"
        end if
    end function generate_code_do_loop

    ! Generate code for do while loop
    function generate_code_do_while(node) result(code)
        type(do_while_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: condition_code, body_code
        integer :: i

        ! Generate condition code
        condition_code = generate_code_polymorphic(node%condition)

        ! Generate body code
        body_code = ""
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                if (allocated(node%body(i)%node)) then
                    body_code = body_code//"    "// &
                                generate_code_polymorphic(node%body(i)%node)// &
                                new_line('a')
                end if
            end do
        end if

        ! Construct do while loop
        code = "do while ("//condition_code//")"//new_line('a')// &
               body_code// &
               "end do"
    end function generate_code_do_while

    ! Generate code for select case node
    function generate_code_select_case(node) result(code)
        type(select_case_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: expr_code, cases_code
        integer :: i, j

        ! Generate expression code
        select type (expr => node%expr)
        type is (literal_node)
            expr_code = generate_code_literal(expr)
        type is (identifier_node)
            expr_code = generate_code_identifier(expr)
        class default
            expr_code = "expr"
        end select

        ! Generate cases
        cases_code = ""
        if (allocated(node%cases)) then
            do i = 1, size(node%cases)
                if (node%cases(i)%case_type == "case_default") then
                    cases_code = cases_code//"case default"//new_line('a')
                else
                    ! Generate case value
                    if (allocated(node%cases(i)%value)) then
                        select type (val => node%cases(i)%value)
                        type is (literal_node)
                            cases_code = cases_code//"case ("// &
                                         generate_code_literal(val)//")"//new_line('a')
                        type is (identifier_node)
                            cases_code = cases_code//"case ("// &
                                       generate_code_identifier(val)//")"//new_line('a')
                        type is (binary_op_node)
                            ! Handle range syntax (2:5)
                            if (val%operator == ":") then
                                cases_code = cases_code//"case ("// &
                                            generate_code_polymorphic(val%left)//":"// &
                                           generate_code_polymorphic(val%right)//")"// &
                                             new_line('a')
                            else
                                cases_code = cases_code//"case ("// &
                                        generate_code_binary_op(val)//")"//new_line('a')
                            end if
                        class default
                            cases_code = cases_code//"case (default)"//new_line('a')
                        end select
                    else
                        cases_code = cases_code//"case default"//new_line('a')
                    end if
                end if

                ! Generate case body
                if (allocated(node%cases(i)%body)) then
                    do j = 1, size(node%cases(i)%body)
                        if (allocated(node%cases(i)%body(j)%node)) then
                            cases_code = cases_code//"    "// &
                               generate_code_polymorphic(node%cases(i)%body(j)%node)// &
                                         new_line('a')
                        end if
                    end do
                end if
            end do
        end if

        ! Construct select case
        code = "select case ("//expr_code//")"//new_line('a')// &
               cases_code// &
               "end select"
    end function generate_code_select_case

end module codegen_core
