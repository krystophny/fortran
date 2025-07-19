module parser_expressions_module
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_NUMBER, TK_STRING, TK_IDENTIFIER, TK_OPERATOR, TK_KEYWORD
    use ast_core
    use ast_factory, only: push_binary_op, push_literal, push_identifier, push_call_or_subscript
    use parser_state_module, only: parser_state_t, create_parser_state
    implicit none
    private

    ! Public expression parsing interface
    public :: parse_expression
    public :: parse_logical_or, parse_logical_and, parse_comparison
    public :: parse_member_access, parse_term, parse_factor, parse_primary

contains

    ! Main expression parsing entry point with stack
    function parse_expression(tokens, arena) result(expr_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        type(parser_state_t) :: parser

        parser = create_parser_state(tokens)
        expr_index = parse_logical_or(parser, arena)
    end function parse_expression

    ! Parse logical OR operators (lowest precedence)
    function parse_logical_or(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token

        expr_index = parse_logical_and(parser, arena)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == ".or.") then
                op_token = parser%consume()  ! consume operator
                right_index = parse_logical_and(parser, arena)
                if (right_index > 0) then
              expr_index = push_binary_op(arena, expr_index, right_index, op_token%text)
                else
                    exit
                end if
            else
                exit
            end if
        end do
    end function parse_logical_or

    ! Parse logical AND operators
    function parse_logical_and(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token

        expr_index = parse_comparison(parser, arena)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == ".and.") then
                op_token = parser%consume()  ! consume operator
                right_index = parse_comparison(parser, arena)
                if (right_index > 0) then
              expr_index = push_binary_op(arena, expr_index, right_index, op_token%text)
                else
                    exit
                end if
            else
                exit
            end if
        end do
    end function parse_logical_and

    ! Parse comparison operators
    function parse_comparison(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token

        expr_index = parse_member_access(parser, arena)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
                (op_token%text == "==" .or. op_token%text == "/=" .or. &
                 op_token%text == "<=" .or. op_token%text == ">=" .or. &
                 op_token%text == "<" .or. op_token%text == ">")) then
                op_token = parser%consume()
                right_index = parse_member_access(parser, arena)
                expr_index = push_binary_op(arena, expr_index, right_index, op_token%text, op_token%line, op_token%column)
            else
                exit
            end if
        end do
    end function parse_comparison

    ! Parse member access operator (%)
    function parse_member_access(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token

        expr_index = parse_term(parser, arena)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == "%") then
                op_token = parser%consume()
                right_index = parse_term(parser, arena)
                expr_index = push_binary_op(arena, expr_index, right_index, op_token%text, op_token%line, op_token%column)
            else
                exit
            end if
        end do
    end function parse_member_access

    ! Parse addition and subtraction
    function parse_term(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token

        expr_index = parse_factor(parser, arena)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
                (op_token%text == "+" .or. op_token%text == "-")) then
                op_token = parser%consume()
                right_index = parse_factor(parser, arena)
                expr_index = push_binary_op(arena, expr_index, right_index, op_token%text, op_token%line, op_token%column)
            else
                exit
            end if
        end do
    end function parse_term

    ! Parse multiplication, division, and power
    function parse_factor(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token

        expr_index = parse_primary(parser, arena)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
       (op_token%text == "*" .or. op_token%text == "/" .or. op_token%text == "**")) then
                op_token = parser%consume()
                right_index = parse_primary(parser, arena)
                expr_index = push_binary_op(arena, expr_index, right_index, op_token%text, op_token%line, op_token%column)
            else
                exit
            end if
        end do
    end function parse_factor

    ! Parse primary expressions (literals, identifiers, parentheses)
    recursive function parse_primary(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        type(token_t) :: current

        current = parser%peek()

        select case (current%kind)
        case (TK_NUMBER)
            ! Parse number literal
            current = parser%consume()
            if (index(current%text, '.') > 0) then
                ! Contains decimal point - classify as real
         expr_index = push_literal(arena, current%text, LITERAL_REAL, current%line, current%column)
            else
                ! No decimal point - classify as integer
      expr_index = push_literal(arena, current%text, LITERAL_INTEGER, current%line, current%column)
            end if

        case (TK_STRING)
            ! Parse string literal
            current = parser%consume()
       expr_index = push_literal(arena, current%text, LITERAL_STRING, current%line, current%column)

        case (TK_IDENTIFIER)
            ! Parse identifier or function call
            current = parser%consume()

            ! Check if followed by '(' for function call
            block
                type(token_t) :: next_token
                character(len=:), allocatable :: func_name
                integer, allocatable :: arg_indices(:)
                type(token_t) :: paren
                integer :: arg_count

                next_token = parser%peek()
                if (next_token%kind == TK_OPERATOR .and. next_token%text == "(") then
                    ! Parse function call

                    func_name = current%text
                    arg_count = 0

                    ! Consume opening paren
                    paren = parser%consume()

                    ! Parse arguments (now handles multiple arguments)
                    next_token = parser%peek()
                    if (next_token%kind /= TK_OPERATOR .or. next_token%text /= ")") then
                        block
                            class(ast_node), allocatable :: arg

                            ! Handle multiple arguments using indices
                            arg_count = 0

                            ! Parse first argument
                            block
                                integer :: arg_index
                                arg_index = parse_comparison(parser, arena)
                                if (arg_index > 0) then
                                    arg_count = 1
                                    allocate (arg_indices(1))
                                    arg_indices(1) = arg_index

                                    ! Parse additional arguments separated by commas
                                    do
                                        next_token = parser%peek()
                    if (next_token%kind /= TK_OPERATOR .or. next_token%text /= ",") exit

                                        ! Consume comma
                                        next_token = parser%consume()

                                        ! Parse next argument
                                        arg_index = parse_comparison(parser, arena)
                                        if (arg_index > 0) then
                                            ! Extend index array
                                            arg_indices = [arg_indices, arg_index]
                                            arg_count = arg_count + 1
                                        else
                                            exit
                                        end if
                                    end do
                                end if
                            end block
                        end block
                    end if

                    ! Consume closing paren if present
                    next_token = parser%peek()
                   if (next_token%kind == TK_OPERATOR .and. next_token%text == ")") then
                        paren = parser%consume()
                    end if

                    ! Create function call node
                    if (allocated(arg_indices)) then
                        block
                            type(call_or_subscript_node) :: call_node
                            call_node = create_call_or_subscript(func_name, arg_indices, current%line, current%column)
                            call arena%push(call_node, "call_or_subscript")
                            expr_index = arena%size
                        end block
                    else
                        ! For empty args, create empty function call
                        block
                            integer, allocatable :: empty_args(:)
                            type(call_or_subscript_node) :: call_node
                            allocate (empty_args(0))  ! Empty index array
                            call_node = create_call_or_subscript(func_name, empty_args, current%line, current%column)
                            call arena%push(call_node, "call_or_subscript")
                            expr_index = arena%size
                        end block
                    end if
                else
         expr_index = push_identifier(arena, current%text, current%line, current%column)
                end if
            end block

        case (TK_OPERATOR)
            ! Check for parentheses
            if (current%text == "(") then
                current = parser%consume()  ! consume '('
                expr_index = parse_comparison(parser, arena)  ! parse the expression inside
                current = parser%peek()
                if (current%text == ")") then
                    current = parser%consume()  ! consume ')'
                end if
            else if (current%text == "-" .or. current%text == "+") then
                ! Unary operator
                block
                    type(token_t) :: op_token
                    integer :: operand_index
                    op_token = parser%consume()
                    operand_index = parse_primary(parser, arena)
                    if (operand_index > 0) then
                        ! Create unary expression as binary op with zero
                        if (op_token%text == "-") then
                            ! For unary minus, create 0 - operand
                            block
                                integer :: zero_index
  zero_index = push_literal(arena, "0", LITERAL_INTEGER, op_token%line, op_token%column)
                      expr_index = push_binary_op(arena, zero_index, operand_index, "-")
                            end block
                        else
                            ! For unary plus, just return the operand
                            expr_index = operand_index
                        end if
                    else
                        expr_index = 0
                    end if
                end block
            else if (current%text == ".not.") then
                ! Logical NOT operator
                block
                    type(token_t) :: op_token
                    integer :: operand_index
                    op_token = parser%consume()
                    operand_index = parse_primary(parser, arena)
                    if (operand_index > 0) then
                        ! Create unary NOT expression as binary op with false
                        block
                            integer :: false_index
             false_index = push_literal(arena, ".false.", LITERAL_LOGICAL, op_token%line, op_token%column)
                 expr_index = push_binary_op(arena, false_index, operand_index, ".not.")
                        end block
                    else
                        expr_index = 0
                    end if
                end block
            else if (current%text == ".") then
                ! Check for logical literals (.true. or .false.)
                block
                    type(token_t) :: next_token, third_token
                    if (parser%current_token + 2 <= size(parser%tokens)) then
                        next_token = parser%tokens(parser%current_token + 1)
                        third_token = parser%tokens(parser%current_token + 2)

                        if (next_token%kind == TK_IDENTIFIER .and. &
                     third_token%kind == TK_OPERATOR .and. third_token%text == ".") then
                     if (next_token%text == "true" .or. next_token%text == "false") then
                                ! It's a logical literal
                                current = parser%consume()  ! consume first '.'
                                current = parser%consume()  ! consume 'true'/'false'
                                current = parser%consume()  ! consume second '.'
    expr_index = push_literal(arena, "."//trim(next_token%text)//".", LITERAL_LOGICAL, &
                                                          current%line, current%column)
                            else
                                ! Not a logical literal
      expr_index = push_literal(arena, "", LITERAL_STRING, current%line, current%column)
                                current = parser%consume()
                            end if
                        else
                            ! Not a logical literal pattern
      expr_index = push_literal(arena, "", LITERAL_STRING, current%line, current%column)
                            current = parser%consume()
                        end if
                    else
                        ! Not enough tokens
      expr_index = push_literal(arena, "", LITERAL_STRING, current%line, current%column)
                        current = parser%consume()
                    end if
                end block
            else
                ! Unrecognized operator - create error node
expr_index = push_literal(arena, "!ERROR: Unrecognized operator '"//current%text//"'", &
                                          LITERAL_STRING, current%line, current%column)
                current = parser%consume()
            end if

        case (TK_KEYWORD)
            ! Handle logical constants
            current = parser%consume()
            if (current%text == ".true." .or. current%text == ".false.") then
      expr_index = push_literal(arena, current%text, LITERAL_LOGICAL, current%line, current%column)
            else
                ! Other keywords - create error node
      expr_index = push_literal(arena, "!ERROR: Unexpected keyword '"//current%text//"' in expression", &
                                          LITERAL_STRING, current%line, current%column)
            end if

        case default
            ! Unrecognized token - create error node and skip
      expr_index = push_literal(arena, "!ERROR: Unrecognized token in expression", LITERAL_STRING, current%line, current%column)
            current = parser%consume()
        end select
    end function parse_primary

end module parser_expressions_module
