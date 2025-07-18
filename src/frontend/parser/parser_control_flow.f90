module parser_control_flow_module
    ! Parser module for control flow constructs (if, do, select case)
    use lexer_core
    use ast_types, only: LITERAL_STRING
    use parser_state_module
  use parser_expressions_module, only: parse_primary, parse_expression, parse_logical_or
    use ast_core
    use ast_factory, only: push_if, push_do_loop, push_do_while, push_select_case, &
                           push_assignment, push_identifier, push_literal
    implicit none
    private

    public :: parse_if, parse_do_loop, parse_do_while, parse_select_case
    public :: parse_if_condition, parse_if_body, parse_elseif_block
    public :: parse_do_while_from_do, parse_basic_statement

contains

    ! Parse if statement
    function parse_if(parser, arena) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: if_index

        type(token_t) :: if_token, then_token
        integer :: condition_index
        integer, allocatable :: then_body_indices(:), else_body_indices(:)
        integer, allocatable :: elseif_indices(:)
        integer :: elseif_count

        ! Consume 'if' keyword
        if_token = parser%consume()

        ! Parse condition (should be in parentheses for standard if/then/endif)
        condition_index = parse_if_condition(parser, arena)

        ! Look for 'then' keyword
        then_token = parser%peek()
        if (then_token%kind == TK_KEYWORD .and. then_token%text == "then") then
            ! Standard if/then/endif block
            then_token = parser%consume()

            ! Parse then body statements
            then_body_indices = parse_if_body(parser, arena)

            ! Check for elseif/else blocks
            elseif_count = 0
            allocate (elseif_indices(0))

            do while (.not. parser%is_at_end())
                then_token = parser%peek()

                if (then_token%kind == TK_KEYWORD) then
                 if (then_token%text == "elseif" .or. then_token%text == "else if") then
                        ! Parse elseif block
                        elseif_count = elseif_count + 1
                        ! Note: elseif handling needs special treatment - skipping for now
                    else if (then_token%text == "else") then
                        ! Parse else block
                        then_token = parser%consume()  ! consume 'else'
                        else_body_indices = parse_if_body(parser, arena)
                        exit
              else if (then_token%text == "endif" .or. then_token%text == "end if") then
                        ! End of if statement
                        then_token = parser%consume()
                        exit
                    else
                        ! Other statement, stop parsing if block
                        exit
                    end if
                else
                    ! Not a keyword, continue parsing body
                    exit
                end if
            end do

            ! Create if node
            if_index = push_if(arena, condition_index, then_body_indices, &
                               else_body_indices=else_body_indices, &
                               line=if_token%line, column=if_token%column)
        else
            ! One-line if statement (no then keyword)
            allocate (then_body_indices(1))

            ! Parse the single statement
            block
                integer :: stmt_index
                type(token_t), allocatable :: remaining_tokens(:)
                integer :: i, n

                ! Count remaining tokens
                n = 0
                do i = parser%current_token, size(parser%tokens)
                    n = n + 1
                end do

                ! Extract remaining tokens
                allocate (remaining_tokens(n))
                remaining_tokens = parser%tokens(parser%current_token:)

                ! Use parse_basic_statement instead of parse_statement
                stmt_index = parse_basic_statement(remaining_tokens, arena)
                if (stmt_index > 0) then
                    then_body_indices(1) = stmt_index
                end if
            end block

            ! Create if node with no elseif/else blocks
            allocate (elseif_indices(0))
            allocate (else_body_indices(0))
            if_index = push_if(arena, condition_index, then_body_indices, &
                               else_body_indices=else_body_indices, &
                               line=if_token%line, column=if_token%column)
        end if

    end function parse_if

    ! Parse if condition (handles parentheses if present)
    function parse_if_condition(parser, arena) result(condition_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: condition_index
        type(token_t) :: paren_token
        type(token_t), allocatable :: remaining_tokens(:)
        integer :: i, n

        ! Check for opening parenthesis
        paren_token = parser%peek()
        if (paren_token%kind == TK_OPERATOR .and. paren_token%text == "(") then
            paren_token = parser%consume()  ! consume '('

            ! Count remaining tokens
            n = 0
            do i = parser%current_token, size(parser%tokens)
                n = n + 1
            end do

            ! Extract remaining tokens
            allocate (remaining_tokens(n))
            remaining_tokens = parser%tokens(parser%current_token:)

            ! Parse the condition expression
            condition_index = parse_expression(remaining_tokens, arena)

            ! Consume closing parenthesis
            paren_token = parser%peek()
            if (paren_token%kind == TK_OPERATOR .and. paren_token%text == ")") then
                paren_token = parser%consume()
            end if
        else
            ! No parentheses, just parse the expression
            ! Count remaining tokens
            n = 0
            do i = parser%current_token, size(parser%tokens)
                n = n + 1
            end do

            ! Extract remaining tokens
            allocate (remaining_tokens(n))
            remaining_tokens = parser%tokens(parser%current_token:)

            condition_index = parse_expression(remaining_tokens, arena)
        end if

    end function parse_if_condition

    ! Parse if/elseif/else body statements
    function parse_if_body(parser, arena) result(body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: body_indices(:)
        type(token_t) :: token
        type(token_t), allocatable :: remaining_tokens(:)
        integer :: stmt_count, i, n
        integer :: stmt_index

        allocate (body_indices(0))
        stmt_count = 0

        ! Use parse_basic_statement instead of parse_statement

        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end of body
            if (token%kind == TK_KEYWORD) then
                if (token%text == "elseif" .or. token%text == "else if" .or. &
                    token%text == "else" .or. token%text == "endif" .or. &
                    token%text == "end if") then
                    exit
                end if
            end if

            ! Count remaining tokens
            n = 0
            do i = parser%current_token, size(parser%tokens)
                n = n + 1
            end do

            ! Extract remaining tokens
            allocate (remaining_tokens(n))
            remaining_tokens = parser%tokens(parser%current_token:)

            ! Parse statement
            stmt_index = parse_basic_statement(remaining_tokens, arena)
            if (stmt_index > 0) then
                stmt_count = stmt_count + 1
                body_indices = [body_indices, stmt_index]
            end if

            deallocate (remaining_tokens)
        end do

    end function parse_if_body

    ! Parse elseif block
    function parse_elseif_block(parser, arena) result(elseif_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: elseif_indices(2) ! condition_index, body_indices_start
        type(token_t) :: elseif_token

        ! Consume 'elseif' or 'else if'
        elseif_token = parser%consume()

        ! Parse condition
        elseif_indices(1) = parse_if_condition(parser, arena)

        ! Look for 'then' keyword
        elseif_token = parser%peek()
        if (elseif_token%kind == TK_KEYWORD .and. elseif_token%text == "then") then
            elseif_token = parser%consume()
        end if

        ! Parse body
        ! Note: This would need special handling for body indices
        ! For now, we'll skip proper elseif implementation

    end function parse_elseif_block

    ! Add other control flow parsing functions here...
    ! Due to length, I'll create placeholders that can be filled with the actual implementations

    function parse_do_loop(parser, arena) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: loop_index

        type(token_t) :: do_token, var_token, eq_token, comma_token
        character(len=:), allocatable :: var_name
        integer :: start_index, end_index, step_index
        integer :: line, column

        step_index = 0  ! Initialize to 0 (no step)

        ! Starting to parse do loop

        ! Consume 'do'
        do_token = parser%consume()
        line = do_token%line
        column = do_token%column

        ! Check if it's a do while loop
        var_token = parser%peek()
        if (var_token%kind == TK_KEYWORD .and. var_token%text == "while") then
            ! Parse as do while loop
            loop_index = parse_do_while_from_do(parser, arena, line, column)
            return
        end if

        ! Get variable name
        var_token = parser%consume()
        if (var_token%kind /= TK_IDENTIFIER) then
            ! Error: expected identifier
            ! ERROR - expected identifier
            return
        end if
        var_name = var_token%text
        ! Got variable name

        ! Expect '='
        eq_token = parser%consume()
        if (eq_token%kind /= TK_OPERATOR .or. eq_token%text /= "=") then
            ! Error: expected '='
            return
        end if

        ! Parse start expression (simplified - just parse next token as literal)
        start_index = parse_primary(parser, arena)

        ! Expect ','
        comma_token = parser%consume()
        if (comma_token%kind /= TK_OPERATOR .or. comma_token%text /= ",") then
            ! Error: expected ','
            return
        end if

        ! Parse end expression
        end_index = parse_primary(parser, arena)

        ! Check for optional step
        if (.not. parser%is_at_end()) then
            comma_token = parser%peek()
            if (comma_token%kind == TK_OPERATOR .and. comma_token%text == ",") then
                comma_token = parser%consume()  ! consume comma
                step_index = parse_primary(parser, arena)
            end if
        end if

        ! Parse body statements until 'end do'
        block
            integer, allocatable :: body_indices(:)
            integer :: stmt_index
            integer :: body_count, stmt_start, stmt_end, j
            type(token_t), allocatable :: stmt_tokens(:)

            allocate (body_indices(0))
            body_count = 0

            ! Parse body statements
            do while (parser%current_token <= size(parser%tokens))
                ! Check for 'end do'
                block
                    type(token_t) :: current_token
                    current_token = parser%peek()

            if (current_token%kind == TK_KEYWORD .and. current_token%text == "end") then
                        if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                              parser%tokens(parser%current_token + 1)%text == "do") then
                                ! Found 'end do', consume both tokens and exit
                                current_token = parser%consume()  ! consume 'end'
                                current_token = parser%consume()  ! consume 'do'
                                exit
                            end if
                        end if
                    end if
                end block

                ! Parse statement until end of line
                stmt_start = parser%current_token
                stmt_end = stmt_start

                ! Find end of current statement (same line)
                do j = stmt_start, size(parser%tokens)
                    if (parser%tokens(j)%kind == TK_EOF) then
                        stmt_end = j
                        exit
                    end if
   if (j > stmt_start .and. parser%tokens(j)%line > parser%tokens(stmt_start)%line) then
                        stmt_end = j - 1
                        exit
                    end if
                    stmt_end = j
                end do

                ! Extract statement tokens
                if (stmt_end >= stmt_start) then
                    allocate (stmt_tokens(stmt_end - stmt_start + 2))
           stmt_tokens(1:stmt_end - stmt_start + 1) = parser%tokens(stmt_start:stmt_end)
                    ! Add EOF token
                    stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                    stmt_tokens(stmt_end - stmt_start + 2)%text = ""
              stmt_tokens(stmt_end - stmt_start + 2)%line = parser%tokens(stmt_end)%line
      stmt_tokens(stmt_end - stmt_start + 2)%column = parser%tokens(stmt_end)%column + 1

                    ! Parse the statement
                    stmt_index = parse_basic_statement(stmt_tokens, arena)

                    ! Add to body if successfully parsed
                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                        body_count = body_count + 1
                    end if

                    deallocate (stmt_tokens)
                end if

                ! Move to next statement
                parser%current_token = stmt_end + 1
            end do

            ! Create do loop node with body
            if (step_index > 0) then
                loop_index = push_do_loop(arena, var_name, start_index, end_index, &
                                     step_index=step_index, body_indices=body_indices, &
                                          line=line, column=column)
            else
                loop_index = push_do_loop(arena, var_name, start_index, end_index, &
                                    body_indices=body_indices, line=line, column=column)
            end if
            ! Successfully created do loop node
        end block

    end function parse_do_loop

    function parse_do_while(parser, arena) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: loop_index

        type(token_t) :: do_token
        integer :: line, column

        ! Consume 'do'
        do_token = parser%consume()
        line = do_token%line
        column = do_token%column

        loop_index = parse_do_while_from_do(parser, arena, line, column)
    end function parse_do_while

    function parse_select_case(parser, arena) result(select_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: select_index

        type(token_t) :: select_token, case_token, lparen_token, rparen_token
        integer :: expr_index
        integer, allocatable :: case_indices(:)
        integer :: case_count, line, column

        ! Consume 'select'
        select_token = parser%consume()
        line = select_token%line
        column = select_token%column

        ! Expect 'case'
        case_token = parser%consume()
        if (case_token%kind /= TK_KEYWORD .or. case_token%text /= "case") then
            ! Error: expected 'case' after 'select'
            return
        end if

        ! Expect '('
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") then
            ! Error: expected '(' after 'select case'
            return
        end if

        ! Parse expression to match
        expr_index = parse_expression(parser%tokens(parser%current_token:), arena)
        if (expr_index <= 0) then
            ! Error: expected expression in select case
            select_index = 0
            return
        end if

        ! Advance parser past the expression
        ! For now, assume expression consumes tokens until ')'
        do while (parser%current_token <= size(parser%tokens))
            rparen_token = parser%peek()
            if (rparen_token%kind == TK_OPERATOR .and. rparen_token%text == ")") then
                rparen_token = parser%consume()
                exit
            end if
            parser%current_token = parser%current_token + 1
        end do

        ! Parse case blocks
        allocate (case_indices(0))
        case_count = 0

        do while (parser%current_token <= size(parser%tokens))
            case_token = parser%peek()

            if (case_token%kind == TK_KEYWORD) then
                if (case_token%text == "case") then
                    ! Parse case block
                    block
                        type(case_wrapper) :: new_case
                        type(token_t) :: value_token
                        class(ast_node), allocatable :: case_value

                        case_token = parser%consume()  ! consume 'case'

                        ! Check for default case
                        value_token = parser%peek()
            if (value_token%kind == TK_KEYWORD .and. value_token%text == "default") then
                            value_token = parser%consume()  ! consume 'default'
                            new_case%case_type = "case_default"
                        else
                            new_case%case_type = "case"

                            ! Expect '('
                 if (value_token%kind == TK_OPERATOR .and. value_token%text == "(") then
                                value_token = parser%consume()  ! consume '('

                                ! Parse case value
                                ! Note: Proper case handling needs special implementation
                                ! For now, skip case value parsing

                                ! Expect ')'
                                value_token = parser%peek()
                 if (value_token%kind == TK_OPERATOR .and. value_token%text == ")") then
                                    value_token = parser%consume()  ! consume ')'
                                end if
                            end if
                        end if

                        ! Parse case body (for now, empty)
                        allocate (new_case%body(0))

                        ! Add to cases array
                        case_count = case_count + 1
                        ! Note: Proper case handling needs to be implemented
                        ! For now, just track the count
                    end block
                else if (case_token%text == "end") then
                    ! Check for 'end select'
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                          parser%tokens(parser%current_token + 1)%text == "select") then
                            ! Found 'end select', consume both tokens and exit
                            case_token = parser%consume()  ! consume 'end'
                            case_token = parser%consume()  ! consume 'select'
                            exit
                        end if
                    end if
                else
                    ! Other keyword, skip
                    parser%current_token = parser%current_token + 1
                end if
            else
                ! Not a keyword, skip
                parser%current_token = parser%current_token + 1
            end if
        end do

        ! Create select case node
        select_index = push_select_case(arena, expr_index, line=line, column=column)

        if (allocated(case_indices)) deallocate (case_indices)
    end function parse_select_case

    ! Helper function to parse basic statements (simplified version)
    function parse_basic_statement(tokens, arena) result(stmt_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(parser_state_t) :: parser
        type(token_t) :: first_token

        parser = create_parser_state(tokens)
        first_token = parser%peek()
        stmt_index = 0  ! Initialize to 0 (no statement)

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
                        stmt_index = push_assignment(arena, target_index, value_index, &
                                                         id_token%line, id_token%column)
                            end if
                        end if
                    end block
                end if
            end block
        end if

        ! If we couldn't parse it, create a placeholder
        if (stmt_index == 0) then
            stmt_index = push_literal(arena, "! Unparsed statement", LITERAL_STRING, &
                                      first_token%line, first_token%column)
        end if

    end function parse_basic_statement

    ! Helper function for parsing do while from do token
    function parse_do_while_from_do(parser, arena, line, column) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: line, column
        integer :: loop_index

        type(token_t) :: while_token, lparen_token, rparen_token
        integer :: condition_index

        ! Consume 'while'
        while_token = parser%consume()
        if (while_token%kind /= TK_KEYWORD .or. while_token%text /= "while") then
            ! Error: expected 'while'
            return
        end if

        ! Expect '('
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") then
            ! Error: expected '('
            return
        end if

        ! Parse condition
        condition_index = parse_logical_or(parser, arena)

        ! Expect ')'
        rparen_token = parser%consume()
        if (rparen_token%kind /= TK_OPERATOR .or. rparen_token%text /= ")") then
            ! Error: expected ')'
            return
        end if

        ! Parse body statements until 'end do'
        block
            integer, allocatable :: body_indices(:)
            integer :: stmt_index
            integer :: body_count, stmt_start, stmt_end, j
            type(token_t), allocatable :: stmt_tokens(:)

            allocate (body_indices(0))
            body_count = 0

            ! Parse body statements
            ! Starting do while body parsing
            do while (parser%current_token <= size(parser%tokens))
                ! Check for 'end do'
                block
                    type(token_t) :: current_token
                    current_token = parser%peek()

            if (current_token%kind == TK_KEYWORD .and. current_token%text == "end") then
                        if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                              parser%tokens(parser%current_token + 1)%text == "do") then
                                ! Found 'end do', consume both tokens and exit
                                while_token = parser%consume()  ! consume 'end'
                                while_token = parser%consume()  ! consume 'do'
                                exit
                            end if
                        end if
                    end if
                end block

                ! Parse a statement
                stmt_start = parser%current_token
                stmt_end = stmt_start

                ! Find end of statement (next line or EOF)
                do j = stmt_start, size(parser%tokens)
   if (j > stmt_start .and. parser%tokens(j)%line > parser%tokens(stmt_start)%line) exit
                    if (parser%tokens(j)%kind == TK_EOF) exit
                    stmt_end = j
                end do

                if (stmt_end >= stmt_start) then
                    allocate (stmt_tokens(stmt_end - stmt_start + 2))
           stmt_tokens(1:stmt_end - stmt_start + 1) = parser%tokens(stmt_start:stmt_end)
                    stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                    stmt_tokens(stmt_end - stmt_start + 2)%text = ""

                    stmt_index = parse_basic_statement(stmt_tokens, arena)

                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                        body_count = body_count + 1
                    end if

                    deallocate (stmt_tokens)
                end if

                parser%current_token = stmt_end + 1
            end do

            ! Create do while node
         loop_index = push_do_while(arena, condition_index, body_indices=body_indices, &
                                       line=line, column=column)

            if (allocated(body_indices)) deallocate (body_indices)
        end block

    end function parse_do_while_from_do

end module parser_control_flow_module
