module parser_statements_module
    ! Parser module for various statement types (use, include, print, etc.)
    use lexer_core
    use parser_state_module
    use parser_expressions_module
    use ast_core
    use ast_factory
    implicit none
    private

    public :: parse_use_statement, parse_include_statement, parse_print_statement
    public :: parse_function_definition, parse_subroutine_definition
    public :: parse_interface_block, parse_module

contains

    ! Parse use statement: use module_name [, only: item1, item2, new_name => old_name]
    function parse_use_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: only_list(:)
        character(len=:), allocatable :: rename_list(:)
        logical :: has_only
        integer :: line, column

        ! Consume 'use' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get module name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            module_name = token%text
        else
            ! Invalid use statement - return placeholder
            stmt_index = push_literal(arena, "! Invalid use statement", LITERAL_STRING, token%line, token%column)
            return
        end if

        has_only = .false.

        ! Check for optional only clause
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()  ! consume ','

            ! Check for 'only' keyword
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "only") then
                token = parser%consume()  ! consume 'only'
                has_only = .true.

                ! Expect ':'
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ":") then
                    token = parser%consume()  ! consume ':'

                    ! Parse only list items
                    call parse_only_list(parser, only_list, rename_list)
                end if
            end if
        end if

        ! Create use statement node
        ! For now, create a placeholder since we don't have push_use_statement yet
     stmt_index = push_literal(arena, "use "//module_name, LITERAL_STRING, line, column)

    end function parse_use_statement

    ! Parse include statement: include 'filename'
    function parse_include_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: filename
        integer :: line, column

        ! Consume "include" keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Expect filename (string literal)
        token = parser%consume()
        if (token%kind == TK_STRING) then
            filename = token%text
        else
            ! Error handling - for now just use empty string
            filename = ""
        end if

        ! Create include statement node
        ! For now, create a placeholder since we don't have push_include_statement yet
        stmt_index = push_literal(arena, "include '"//filename//"'", LITERAL_STRING, line, column)

    end function parse_include_statement

    ! Parse print statement: print format_spec, arg1, arg2, ...
    function parse_print_statement(parser, arena) result(print_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: print_index

        type(token_t) :: token
        integer, allocatable :: arg_indices(:)
        integer :: line, column, arg_count
        character(len=:), allocatable :: format_spec

        ! Consume 'print' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()

        ! Parse format spec (*, format string, or format variable)
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "*") then
            format_spec = "*"
            token = parser%consume()
        else if (token%kind == TK_STRING) then
            ! Format string like '(a,f5.1,a,f5.1,a,f5.1)'
            format_spec = token%text
            token = parser%consume()
        else if (token%kind == TK_IDENTIFIER) then
            ! Format variable
            format_spec = token%text
            token = parser%consume()
        else
            format_spec = "*"  ! Default
        end if

        ! Skip comma after format spec if present
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()
        end if

        ! Parse all print arguments
        arg_count = 0
        allocate (arg_indices(0))

        if (.not. parser%is_at_end()) then
            block
                integer :: current_arg_index

                ! Parse first argument
                current_arg_index = parse_comparison(parser, arena)
                if (current_arg_index > 0) then
                    arg_count = 1
                    arg_indices = [current_arg_index]

                    ! Parse additional arguments separated by commas
                    do
                        token = parser%peek()
                        if (token%kind /= TK_OPERATOR .or. token%text /= ",") exit

                        ! Consume comma
                        token = parser%consume()

                        ! Parse next argument
                        current_arg_index = parse_comparison(parser, arena)
                        if (current_arg_index > 0) then
                            arg_indices = [arg_indices, current_arg_index]
                            arg_count = arg_count + 1
                        else
                            exit
                        end if
                    end do
                end if
            end block
        end if

        ! Create print statement node
        ! For now, create a placeholder since we don't have push_print_statement yet
        if (arg_count > 0) then
            print_index = push_literal(arena, "print "//format_spec//", ...", LITERAL_STRING, line, column)
        else
  print_index = push_literal(arena, "print "//format_spec, LITERAL_STRING, line, column)
        end if

    end function parse_print_statement

    ! Parse only list helper
    subroutine parse_only_list(parser, only_list, rename_list)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: only_list(:)
        character(len=:), allocatable, intent(out) :: rename_list(:)

        character(len=100) :: temp_only(50)
        character(len=100) :: temp_rename(50)
        type(token_t) :: token
        integer :: count
        character(len=:), allocatable :: item_name, old_name

        count = 0
        temp_only = ""
        temp_rename = ""

        ! Parse first item
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            item_name = token%text

            ! Check if this is a rename (new_name => old_name)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "=") then
                token = parser%consume()  ! consume '='
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ">") then
                    token = parser%consume()  ! consume '>'

                    ! Get old name
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        token = parser%consume()
                        old_name = token%text

                        ! Store rename: "new_name => old_name"
                        count = count + 1
                        temp_rename(count) = item_name//" => "//old_name
                    end if
                end if
            else
                ! Regular only item
                count = count + 1
                temp_only(count) = item_name
            end if
        end if

        ! Parse remaining items
        do
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume ','

                ! Get next item
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    token = parser%consume()
                    item_name = token%text

                    ! Check if this is a rename
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == "=") then
                        token = parser%consume()  ! consume '='
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == ">") then
                            token = parser%consume()  ! consume '>'

                            ! Get old name
                            token = parser%peek()
                            if (token%kind == TK_IDENTIFIER) then
                                token = parser%consume()
                                old_name = token%text

                                ! Store rename: "new_name => old_name"
                                count = count + 1
                                temp_rename(count) = item_name//" => "//old_name
                            end if
                        end if
                    else
                        ! Regular only item
                        count = count + 1
                        temp_only(count) = item_name
                    end if
                else
                    ! No more items
                    exit
                end if
            else
                ! No more items
                exit
            end if
        end do

        ! Copy to output arrays with proper size
        block
            integer :: only_count, rename_count, i

            ! Count actual items
            only_count = 0
            rename_count = 0
            do i = 1, count
                if (len_trim(temp_only(i)) > 0) only_count = only_count + 1
                if (len_trim(temp_rename(i)) > 0) rename_count = rename_count + 1
            end do

            ! Allocate output arrays
            if (only_count > 0) then
                allocate (character(len=100) :: only_list(only_count))
                only_count = 0
                do i = 1, count
                    if (len_trim(temp_only(i)) > 0) then
                        only_count = only_count + 1
                        only_list(only_count) = trim(temp_only(i))
                    end if
                end do
            end if

            if (rename_count > 0) then
                allocate (character(len=100) :: rename_list(rename_count))
                rename_count = 0
                do i = 1, count
                    if (len_trim(temp_rename(i)) > 0) then
                        rename_count = rename_count + 1
                        rename_list(rename_count) = trim(temp_rename(i))
                    end if
                end do
            end if
        end block

    end subroutine parse_only_list

    ! Parse derived type definition: type :: type_name or type(params) :: type_name
    function parse_derived_type(parser, arena) result(type_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: type_index

        type(token_t) :: token
        character(len=:), allocatable :: type_name
        integer :: line, column
        logical :: has_parameters
        integer, allocatable :: param_indices(:)

        ! Consume 'type' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        has_parameters = .false.

        ! Check for :: or just get type name
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "::") then
            ! Consume ::
            token = parser%consume()

            ! Get type name
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER) then
                token = parser%consume()
                type_name = token%text

                ! Check for parameters after type name: type :: matrix(n, m)
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "(") then
                    ! Parse parameters
                    has_parameters = .true.
                    token = parser%consume()  ! consume '('
                    call parse_derived_type_parameters(parser, arena, param_indices)

                    ! Consume ')'
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == ")") then
                        token = parser%consume()
                    end if
                end if
            else
                type_name = "unnamed_type"
            end if
        else if (token%kind == TK_IDENTIFIER) then
            ! Direct type name (like "type point")
            token = parser%consume()
            type_name = token%text

            ! Check for parameters after type name: type matrix(n, m)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                ! Parse parameters
                has_parameters = .true.
                token = parser%consume()  ! consume '('
                call parse_derived_type_parameters(parser, arena, param_indices)

                ! Consume ')'
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                end if
            end if
        else
            type_name = "unnamed_type"
        end if

        ! Create derived type node
        if (has_parameters .and. allocated(param_indices)) then
            type_index = push_derived_type(arena, type_name, param_indices=param_indices, line=line, column=column)
        else
            type_index = push_derived_type(arena, type_name, line=line, column=column)
        end if

    end function parse_derived_type

    ! Parse derived type parameters inside parentheses
    subroutine parse_derived_type_parameters(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: param_indices(:)

        type(token_t) :: token
        integer :: param_count

        ! Initialize
        param_count = 0
        allocate (param_indices(0))

        ! Parse parameters separated by commas
        do
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                ! End of parameters
                exit
            end if

            ! Parse a single parameter
            if (token%kind == TK_IDENTIFIER) then
                token = parser%consume()
                param_indices = [param_indices, push_identifier(arena, token%text, token%line, token%column)]
                param_count = param_count + 1
            else if (token%kind == TK_NUMBER) then
                token = parser%consume()
                param_indices = [param_indices, push_literal(arena, token%text, LITERAL_INTEGER, token%line, token%column)]
                param_count = param_count + 1
            else
                ! Unknown parameter type - consume it as identifier
                token = parser%consume()
                param_indices = [param_indices, push_identifier(arena, token%text, token%line, token%column)]
                param_count = param_count + 1
            end if

            ! Check for comma
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! Consume comma
                cycle
            else
                exit
            end if
        end do

    end subroutine parse_derived_type_parameters

    function parse_function_definition(parser, arena) result(func_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: func_index

        character(len=:), allocatable :: return_type_str, func_name
        type(token_t) :: token
        integer, allocatable :: param_indices(:), body_indices(:)
        integer :: return_type_index
        integer :: line, column

        ! Initialize
        return_type_str = ""

        ! Check if we have a return type
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. &
            (token%text == "real" .or. token%text == "integer" .or. &
             token%text == "logical" .or. token%text == "character")) then
            return_type_str = token%text
            token = parser%consume()
        end if

        ! Expect "function" keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "function") then
            line = token%line
            column = token%column
            token = parser%consume()
        else
            ! Error - not a function definition
            func_index = push_literal(arena, "! Error: Expected function keyword", LITERAL_STRING, 1, 1)
            return
        end if

        ! Get function name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            func_name = token%text
            token = parser%consume()
        else
            ! Error - missing function name, create empty function
            func_name = "unnamed_function"
        end if

        ! Parse parameters
        ! Look for opening parenthesis
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()

            ! Parse parameter list
            block
                integer :: param_count

                param_count = 0
                allocate (param_indices(0))

                do while (.not. parser%is_at_end())
                    token = parser%peek()

                    ! Check for closing parenthesis
                    if (token%kind == TK_OPERATOR .and. token%text == ")") then
                        token = parser%consume()
                        exit
                    end if

                    ! Check for comma (parameter separator)
                    if (token%kind == TK_OPERATOR .and. token%text == ",") then
                        token = parser%consume()
                        cycle
                    end if

                    ! Parse parameter identifier
                    if (token%kind == TK_IDENTIFIER) then
                        param_indices = [param_indices, push_identifier(arena, token%text, token%line, token%column)]
                        param_count = param_count + 1
                        token = parser%consume()
                    else
                        ! Skip unexpected token
                        token = parser%consume()
                    end if
                end do
            end block
        else
            allocate (param_indices(0))
        end if

        ! Parse function body (collect all statements until "end function")
        block
            integer :: stmt_index
            integer :: body_count, i, stmt_start
            type(token_t), allocatable :: stmt_tokens(:)

            body_count = 0
            allocate (body_indices(0))

            ! Parse statements until we hit "end function"
            do while (.not. parser%is_at_end())
                token = parser%peek()

                ! Check for "end function"
                if (token%kind == TK_KEYWORD .and. token%text == "end") then
                    ! Look ahead for "function"
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                        block
                            type(token_t) :: next_token
                            next_token = parser%tokens(parser%current_token + 1)
             if (next_token%kind == TK_KEYWORD .and. next_token%text == "function") then
                                ! Consume "end function"
                                token = parser%consume()
                                token = parser%consume()
                                exit
                            end if
                        end block
                    end if
                end if

                ! Collect tokens for the current statement (until newline or end of line)
                stmt_start = parser%current_token
                i = stmt_start
                do while (i <= size(parser%tokens))
                    if (parser%tokens(i)%line /= token%line) exit
                    i = i + 1
                end do

                ! Extract statement tokens
                if (i > stmt_start) then
                    allocate (stmt_tokens(i - stmt_start + 1))
                    stmt_tokens(1:i - stmt_start) = parser%tokens(stmt_start:i - 1)
                    ! Add EOF token
                    stmt_tokens(i - stmt_start + 1)%kind = TK_EOF
                    stmt_tokens(i - stmt_start + 1)%text = ""
                    stmt_tokens(i - stmt_start + 1)%line = parser%tokens(i - 1)%line
                stmt_tokens(i - stmt_start + 1)%column = parser%tokens(i - 1)%column + 1

                    ! Parse the statement properly
                    stmt_index = parse_basic_statement(stmt_tokens, arena)

                    ! Only add to body if statement was successfully parsed
                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                        body_count = body_count + 1
                    end if

                    deallocate (stmt_tokens)

                    ! Advance parser to next statement
                    parser%current_token = i
                else
                    ! Skip to next token if we can't parse this statement
                    token = parser%consume()
                end if
            end do
        end block

        ! Create return type node from string
        if (len_trim(return_type_str) > 0) then
            return_type_index = push_identifier(arena, return_type_str, line, column)
        else
            return_type_index = push_identifier(arena, "", line, column)
        end if

        ! Create function definition node
        ! For now, create a placeholder since we don't have push_function_def yet
  func_index = push_literal(arena, "function "//func_name, LITERAL_STRING, line, column)

    end function parse_function_definition

    function parse_subroutine_definition(parser, arena) result(sub_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: sub_index

        character(len=:), allocatable :: sub_name
        type(token_t) :: token
        integer, allocatable :: param_indices(:), body_indices(:)
        integer :: line, column

        ! Expect "subroutine" keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "subroutine") then
            line = token%line
            column = token%column
            token = parser%consume()
        else
            ! Error: expected subroutine
            return
        end if

        ! Get subroutine name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            sub_name = token%text
            token = parser%consume()
        else
            ! Error: expected subroutine name
            return
        end if

        ! Parse parameters
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('

            ! Parse parameter list inline
            block
                integer :: param_count

                param_count = 0
                allocate (param_indices(0))

                do while (.not. parser%is_at_end())
                    token = parser%peek()

                    ! Check for closing parenthesis
                    if (token%kind == TK_OPERATOR .and. token%text == ")") then
                        token = parser%consume()
                        exit
                    end if

                    ! Check for comma (parameter separator)
                    if (token%kind == TK_OPERATOR .and. token%text == ",") then
                        token = parser%consume()
                        cycle
                    end if

                    ! Parse parameter identifier
                    if (token%kind == TK_IDENTIFIER) then
                        param_indices = [param_indices, push_identifier(arena, token%text, token%line, token%column)]
                        param_count = param_count + 1
                        token = parser%consume()
                    else
                        ! Skip unexpected token
                        token = parser%consume()
                    end if
                end do
            end block
        else
            ! No parameters
            allocate (param_indices(0))
        end if

        ! Parse body (simplified for now - just consume tokens until end subroutine)
        allocate (body_indices(0))

        ! Look for end subroutine
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                token = parser%consume()  ! consume 'end'
                token = parser%peek()
                if (token%kind == TK_KEYWORD .and. token%text == "subroutine") then
                    token = parser%consume()  ! consume 'subroutine'
                    exit
                end if
            else
                token = parser%consume()  ! skip other tokens
            end if
        end do

        ! Create subroutine node
        ! For now, create a placeholder since we don't have push_subroutine_def yet
  sub_index = push_literal(arena, "subroutine "//sub_name, LITERAL_STRING, line, column)

    end function parse_subroutine_definition

    function parse_interface_block(parser, arena) result(interface_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: interface_index

        type(token_t) :: interface_token, token
        character(len=:), allocatable :: name, operator_symbol, kind
        integer :: line, column
        integer, allocatable :: procedure_indices(:)

        ! Consume 'interface' keyword
        interface_token = parser%consume()
        line = interface_token%line
        column = interface_token%column
        kind = "interface"

        ! Check what follows the interface keyword
        token = parser%peek()

        if (token%kind == TK_IDENTIFIER) then
            ! Named interface: interface name
            token = parser%consume()
            name = token%text
            kind = "generic"
        else if (token%kind == TK_KEYWORD .and. token%text == "operator") then
            ! Operator interface: interface operator(+)
            token = parser%consume()  ! consume 'operator'
            kind = "operator"

            ! Expect '('
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()  ! consume '('

                ! Get operator symbol
                token = parser%peek()
                if (token%kind == TK_OPERATOR) then
                    operator_symbol = token%text
                    token = parser%consume()
                end if

                ! Expect ')'
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()  ! consume ')'
                end if
            end if
        else if (token%kind == TK_KEYWORD .and. token%text == "assignment") then
            ! Assignment interface: interface assignment(=)
            token = parser%consume()  ! consume 'assignment'
            kind = "assignment"

            ! Expect '(=)'
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()  ! consume '('
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "=") then
                    token = parser%consume()  ! consume '='
                    operator_symbol = "="
                end if
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()  ! consume ')'
                end if
            end if
        end if

        ! TODO: Parse interface body (procedure declarations)
        ! For now, just skip until "end interface"
        do
            token = parser%peek()
            if (token%kind == TK_EOF) exit
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                token = parser%consume()  ! consume 'end'
                token = parser%peek()
                if (token%kind == TK_KEYWORD .and. token%text == "interface") then
                    token = parser%consume()  ! consume 'interface'
                    exit
                end if
            else
                token = parser%consume()  ! skip token
            end if
        end do

        ! Create interface block node
        ! For now, create a placeholder since we don't have push_interface_block yet
        if (allocated(name)) then
 interface_index = push_literal(arena, "interface "//name, LITERAL_STRING, line, column)
        else if (allocated(operator_symbol)) then
            interface_index = push_literal(arena, "interface operator("//operator_symbol//")", LITERAL_STRING, line, column)
        else
        interface_index = push_literal(arena, "interface", LITERAL_STRING, line, column)
        end if

    end function parse_interface_block

    function parse_module(parser, arena) result(module_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: module_index

        type(token_t) :: module_token, token
        character(len=:), allocatable :: name
        integer :: line, column
        integer, allocatable :: declaration_indices(:)
        integer, allocatable :: procedure_indices(:)
        logical :: has_contains

        ! Consume 'module' keyword
        module_token = parser%consume()
        line = module_token%line
        column = module_token%column
        has_contains = .false.

        ! Get module name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            name = token%text
        else
            ! Error: expected module name
            name = "unknown"
        end if

        ! TODO: Parse module body (declarations)
        ! For now, just skip until "contains" or "end module"
        do
            token = parser%peek()
            if (token%kind == TK_EOF) exit
            if (token%kind == TK_KEYWORD .and. token%text == "contains") then
                token = parser%consume()  ! consume 'contains'
                has_contains = .true.
                exit
            else if (token%kind == TK_KEYWORD .and. token%text == "end") then
                token = parser%consume()  ! consume 'end'
                token = parser%peek()
                if (token%kind == TK_KEYWORD .and. token%text == "module") then
                    token = parser%consume()  ! consume 'module'
                    ! Optional module name after 'end module'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        token = parser%consume()  ! consume module name
                    end if
                    exit
                end if
            else
                token = parser%consume()  ! skip token
            end if
        end do

        ! TODO: Parse procedures after contains
        ! For now, just skip until "end module"
        if (has_contains) then
            do
                token = parser%peek()
                if (token%kind == TK_EOF) exit
                if (token%kind == TK_KEYWORD .and. token%text == "end") then
                    token = parser%consume()  ! consume 'end'
                    token = parser%peek()
                    if (token%kind == TK_KEYWORD .and. token%text == "module") then
                        token = parser%consume()  ! consume 'module'
                        ! Optional module name after 'end module'
                        token = parser%peek()
                        if (token%kind == TK_IDENTIFIER) then
                            token = parser%consume()  ! consume module name
                        end if
                        exit
                    end if
                else
                    token = parser%consume()  ! skip token
                end if
            end do
        end if

        ! Create module node
        ! For now, create a placeholder since we don't have push_module yet
       module_index = push_literal(arena, "module "//name, LITERAL_STRING, line, column)

    end function parse_module

    ! Helper function to parse basic statements (simplified version)
    function parse_basic_statement(tokens, arena) result(stmt_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(parser_state_t) :: parser
        type(token_t) :: first_token

        parser = create_parser_state(tokens)
        first_token = parser%peek()
        stmt_index = 0

        ! Simple assignment statement: identifier = expression
        if (first_token%kind == TK_IDENTIFIER) then
            block
                type(token_t) :: id_token, op_token
                integer :: target_index, value_index

                id_token = parser%consume()
                op_token = parser%peek()

                if (op_token%kind == TK_OPERATOR .and. op_token%text == "=") then
                    op_token = parser%consume()  ! consume '='
    target_index = push_identifier(arena, id_token%text, id_token%line, id_token%column)
                    ! Get remaining tokens for expression parsing
                    block
                        type(token_t), allocatable :: expr_tokens(:)
                        integer :: remaining_count
                        remaining_count = size(tokens) - parser%current_token + 1
                        if (remaining_count > 0) then
                            allocate (expr_tokens(remaining_count))
                            expr_tokens = tokens(parser%current_token:)
                            value_index = parse_expression(expr_tokens, arena)
                            if (value_index > 0) then
                                stmt_index = push_assignment(arena, target_index, value_index, id_token%line, id_token%column)
                            end if
                        end if
                    end block
                end if
            end block
        end if

        ! If we couldn't parse it, create a placeholder
        if (stmt_index == 0) then
            stmt_index = push_literal(arena, "! Unparsed statement", LITERAL_STRING, first_token%line, first_token%column)
        end if

    end function parse_basic_statement

end module parser_statements_module
