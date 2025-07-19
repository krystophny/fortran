module ast_factory
    use ast_core
    implicit none
    private

    ! Public interface for creating AST nodes in stack-based system
    public :: push_program, push_assignment, push_binary_op
   public :: push_call_or_subscript, push_subroutine_call, push_identifier, push_literal
    public :: push_derived_type, push_declaration
    public :: push_if, push_do_loop, push_do_while, push_select_case
    public :: push_use_statement, push_include_statement, push_print_statement
    public :: push_function_def, push_subroutine_def, push_interface_block, push_module
    public :: build_ast_from_nodes

contains

    ! Create program node and add to stack
    function push_program(arena, name, body_indices, line, column) result(prog_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: body_indices(:)
        integer, intent(in), optional :: line, column
        integer :: prog_index
        type(program_node) :: prog

        prog = create_program(name, body_indices, line, column)
        call arena%push(prog, "program")
        prog_index = arena%size
    end function push_program

    ! Create assignment node and add to stack
    function push_assignment(arena, target_index, value_index, line, column, parent_index) result(assign_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: target_index, value_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: assign_index
        type(assignment_node) :: assign

        assign = create_assignment(target_index, value_index, line, column)
        call arena%push(assign, "assignment", parent_index)
        assign_index = arena%size
    end function push_assignment

    ! Create binary operation node and add to stack
    function push_binary_op(arena, left_index, right_index, operator, line, column, parent_index) result(binop_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: left_index, right_index
        character(len=*), intent(in) :: operator
        integer, intent(in), optional :: line, column, parent_index
        integer :: binop_index
        type(binary_op_node) :: binop

        binop = create_binary_op(left_index, right_index, operator, line, column)
        call arena%push(binop, "binary_op", parent_index)
        binop_index = arena%size
    end function push_binary_op

    ! Create call_or_subscript node and add to stack
    function push_call_or_subscript(arena, name, arg_indices, line, column, parent_index) result(call_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: call_index
        type(call_or_subscript_node) :: call_node

        call_node = create_call_or_subscript(name, arg_indices, line, column)
        call arena%push(call_node, "call_or_subscript", parent_index)
        call_index = arena%size
    end function push_call_or_subscript

    ! Create subroutine call node and add to stack
    function push_subroutine_call(arena, name, arg_indices, line, column, parent_index) result(call_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: call_index
        type(subroutine_call_node) :: call_node

        call_node = create_subroutine_call(name, arg_indices, line, column)
        call arena%push(call_node, "subroutine_call", parent_index)
        call_index = arena%size
    end function push_subroutine_call

    ! Create identifier node and add to stack
    function push_identifier(arena, name, line, column, parent_index) result(id_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: line, column, parent_index
        integer :: id_index
        type(identifier_node) :: id

        id = create_identifier(name, line, column)
        call arena%push(id, "identifier", parent_index)
        id_index = arena%size
    end function push_identifier

    ! Create literal node and add to stack
 function push_literal(arena, value, kind, line, column, parent_index) result(lit_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: value
        integer, intent(in) :: kind
        integer, intent(in), optional :: line, column, parent_index
        integer :: lit_index
        type(literal_node) :: lit

        lit = create_literal(value, kind, line, column)
        call arena%push(lit, "literal", parent_index)
        lit_index = arena%size
    end function push_literal

    ! Create derived type node and add to stack
    function push_derived_type(arena, name, component_indices, param_indices, &
                               line, column, parent_index) result(type_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: component_indices(:)
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: type_index
        type(derived_type_node) :: dtype

        ! Create derived type with index-based components
        dtype%name = name

        if (present(component_indices)) then
            if (size(component_indices) > 0) then
                allocate (dtype%component_indices, source=component_indices)
            end if
        end if

        if (present(param_indices)) then
            if (size(param_indices) > 0) then
                dtype%has_parameters = .true.
                allocate (dtype%param_indices, source=param_indices)
            end if
        end if

        if (present(line)) dtype%line = line
        if (present(column)) dtype%column = column

        call arena%push(dtype, "derived_type", parent_index)
        type_index = arena%size
    end function push_derived_type

    ! Create declaration node and add to stack
  function push_declaration(arena, type_name, var_name, kind_value, dimension_indices, &
       initializer_index, is_allocatable, line, column, parent_index) result(decl_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name, var_name
        integer, intent(in), optional :: kind_value
        integer, intent(in), optional :: dimension_indices(:)
        integer, intent(in), optional :: initializer_index
        logical, intent(in), optional :: is_allocatable
        integer, intent(in), optional :: line, column, parent_index
        integer :: decl_index
        type(declaration_node) :: decl

        ! Create declaration with index-based fields
        decl%type_name = type_name
        decl%var_name = var_name

        if (present(kind_value)) then
            decl%kind_value = kind_value
            decl%has_kind = .true.
        else
            decl%kind_value = 0
            decl%has_kind = .false.
        end if

        if (present(initializer_index)) then
            decl%initializer_index = initializer_index
            decl%has_initializer = .true.
        else
            decl%initializer_index = 0
            decl%has_initializer = .false.
        end if

        if (present(dimension_indices)) then
            decl%is_array = .true.
            allocate (decl%dimension_indices, source=dimension_indices)
        else
            decl%is_array = .false.
        end if

        if (present(is_allocatable)) then
            decl%is_allocatable = is_allocatable
        else
            decl%is_allocatable = .false.
        end if

        if (present(line)) decl%line = line
        if (present(column)) decl%column = column

        call arena%push(decl, "declaration", parent_index)
        decl_index = arena%size
    end function push_declaration

    ! Create if statement node and add to stack
    function push_if(arena, condition_index, then_body_indices, elseif_indices, else_body_indices, &
                     line, column, parent_index) result(if_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: then_body_indices(:)
        integer, intent(in), optional :: elseif_indices(:)
        integer, intent(in), optional :: else_body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: if_index
        type(if_node) :: if_stmt
        integer :: i

        ! Set condition index
        if (condition_index > 0 .and. condition_index <= arena%size) then
            if_stmt%condition_index = condition_index
        end if

        ! Set then body indices
        if (present(then_body_indices)) then
            if (size(then_body_indices) > 0) then
                if_stmt%then_body_indices = then_body_indices
            end if
        end if

        ! Set else body indices
        if (present(else_body_indices)) then
            if (size(else_body_indices) > 0) then
                if_stmt%else_body_indices = else_body_indices
            end if
        end if

        ! Handle elseif blocks
        if (present(elseif_indices)) then
            if (size(elseif_indices) > 0) then
                ! For now, treat elseif_indices as pairs: condition, body, condition, body, ...
                ! Each pair becomes one elseif_wrapper
                if (mod(size(elseif_indices), 2) == 0) then
                    allocate (if_stmt%elseif_blocks(size(elseif_indices)/2))
                    do i = 1, size(elseif_indices)/2
                      if_stmt%elseif_blocks(i)%condition_index = elseif_indices(2*i - 1)
                        if_stmt%elseif_blocks(i)%body_indices = [elseif_indices(2*i)]
                    end do
                end if
            end if
        end if

        if (present(line)) if_stmt%line = line
        if (present(column)) if_stmt%column = column

        call arena%push(if_stmt, "if_statement", parent_index)
        if_index = arena%size
    end function push_if

    ! Create do loop node and add to stack
    function push_do_loop(arena, var_name, start_index, end_index, step_index, body_indices, &
                          line, column, parent_index) result(loop_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_index, end_index
        integer, intent(in), optional :: step_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: loop_index
        type(do_loop_node) :: loop_node
        integer :: i

        loop_node%var_name = var_name

        ! Set start and end expression indices
        if (start_index > 0 .and. start_index <= arena%size) then
            loop_node%start_expr_index = start_index
        end if

        if (end_index > 0 .and. end_index <= arena%size) then
            loop_node%end_expr_index = end_index
        end if

        ! Set optional step expression index
        if (present(step_index)) then
            if (step_index > 0) then
                loop_node%step_expr_index = step_index
            end if
        end if

        ! Set body indices
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                loop_node%body_indices = body_indices
            end if
        end if

        if (present(line)) loop_node%line = line
        if (present(column)) loop_node%column = column

        call arena%push(loop_node, "do_loop", parent_index)
        loop_index = arena%size
    end function push_do_loop

    ! Create do while loop node and add to stack
    function push_do_while(arena, condition_index, body_indices, line, column, parent_index) result(while_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: while_index
        type(do_while_node) :: while_node
        integer :: i

        ! Set condition index
        if (condition_index > 0 .and. condition_index <= arena%size) then
            while_node%condition_index = condition_index
        end if

        ! Set body indices
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                while_node%body_indices = body_indices
            end if
        end if

        if (present(line)) while_node%line = line
        if (present(column)) while_node%column = column

        call arena%push(while_node, "do_while", parent_index)
        while_index = arena%size
    end function push_do_while

    ! Create select case node and add to stack
    function push_select_case(arena, expr_index, case_indices, line, column, parent_index) result(select_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: expr_index
        integer, intent(in), optional :: case_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: select_index
        type(select_case_node) :: select_node
        integer :: i

        ! Set expression
        if (expr_index > 0 .and. expr_index <= arena%size) then
            if (allocated(arena%entries(expr_index)%node)) then
                allocate (select_node%expr, source=arena%entries(expr_index)%node)
            end if
        end if

        ! Handle case blocks
        if (present(case_indices)) then
            if (size(case_indices) > 0) then
                ! For now, treat case_indices as triplets: case_type, value, body
                ! Each triplet becomes one case_wrapper
                if (mod(size(case_indices), 3) == 0) then
                    allocate (select_node%cases(size(case_indices)/3))
                    do i = 1, size(case_indices)/3
                        ! Case type stored as literal in arena
           if (case_indices(3*i - 2) > 0 .and. case_indices(3*i - 2) <= arena%size) then
               select type (case_type_node => arena%entries(case_indices(3*i - 2))%node)
                            type is (literal_node)
                                select_node%cases(i)%case_type = case_type_node%value
                            end select
                        end if
                        ! Case value
           if (case_indices(3*i - 1) > 0 .and. case_indices(3*i - 1) <= arena%size) then
                          if (allocated(arena%entries(case_indices(3*i - 1))%node)) then
 allocate (select_node%cases(i)%value, source=arena%entries(case_indices(3*i - 1))%node)
                            end if
                        end if
                        ! Case body (simplified - just store single statement)
                   if (case_indices(3*i) > 0 .and. case_indices(3*i) <= arena%size) then
                            allocate (select_node%cases(i)%body(1))
                            if (allocated(arena%entries(case_indices(3*i))%node)) then
                                allocate (select_node%cases(i)%body(1)%node, source=arena%entries(case_indices(3*i))%node)
                            end if
                        end if
                    end do
                end if
            end if
        end if

        if (present(line)) select_node%line = line
        if (present(column)) select_node%column = column

        call arena%push(select_node, "select_case", parent_index)
        select_index = arena%size
    end function push_select_case

    ! Build AST from individual nodes (helper function)
    subroutine build_ast_from_nodes(arena, node_specs, indices)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: node_specs(:)  ! Array of "type:name" specs
        integer, intent(out) :: indices(:)  ! Output indices
        integer :: i

        do i = 1, size(node_specs)
            block
                character(len=:), allocatable :: spec
                integer :: colon_pos
                character(len=:), allocatable :: node_type, node_name

                spec = trim(node_specs(i))
                colon_pos = index(spec, ':')

                if (colon_pos > 0) then
                    node_type = spec(1:colon_pos - 1)
                    node_name = spec(colon_pos + 1:)

                    select case (trim(node_type))
                    case ('identifier')
                        indices(i) = push_identifier(arena, node_name, i, 1)
                    case ('literal_int')
                      indices(i) = push_literal(arena, node_name, LITERAL_INTEGER, i, 1)
                    case ('literal_real')
                        indices(i) = push_literal(arena, node_name, LITERAL_REAL, i, 1)
                    case ('literal_string')
                       indices(i) = push_literal(arena, node_name, LITERAL_STRING, i, 1)
                    case default
                        indices(i) = push_identifier(arena, node_name, i, 1)
                    end select
                else
                    indices(i) = push_identifier(arena, spec, i, 1)
                end if
            end block
        end do
    end subroutine build_ast_from_nodes

    ! Create use statement node and add to stack
    function push_use_statement(arena, module_name, only_list, rename_list, &
                                has_only, line, column, parent_index) result(use_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in), optional :: only_list(:), rename_list(:)
        logical, intent(in), optional :: has_only
        integer, intent(in), optional :: line, column, parent_index
        integer :: use_index
        type(use_statement_node) :: use_stmt

        use_stmt = create_use_statement(module_name, only_list, rename_list, has_only, line, column)
        call arena%push(use_stmt, "use_statement", parent_index)
        use_index = arena%size
    end function push_use_statement

    ! Create include statement node and add to stack
    function push_include_statement(arena, filename, line, column, parent_index) result(include_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: line, column, parent_index
        integer :: include_index
        type(include_statement_node) :: include_stmt

        include_stmt = create_include_statement(filename, line, column)
        call arena%push(include_stmt, "include_statement", parent_index)
        include_index = arena%size
    end function push_include_statement

    ! Create print statement node and add to stack
    function push_print_statement(arena, format_spec, arg_indices, line, column, parent_index) result(print_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: format_spec
        integer, intent(in), optional :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: print_index
        type(print_statement_node) :: print_stmt

        print_stmt%format_spec = format_spec
        if (present(arg_indices)) then
            if (size(arg_indices) > 0) then
                print_stmt%arg_indices = arg_indices
            end if
        end if
        if (present(line)) print_stmt%line = line
        if (present(column)) print_stmt%column = column

        call arena%push(print_stmt, "print_statement", parent_index)
        print_index = arena%size
    end function push_print_statement

    ! Create function definition node and add to stack
    function push_function_def(arena, name, param_indices, return_type, body_indices, &
                               line, column, parent_index) result(func_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        character(len=*), intent(in), optional :: return_type
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: func_index
        type(function_def_node) :: func_def

        func_def = create_function_def(name, param_indices, return_type, body_indices, line, column)
        call arena%push(func_def, "function_def", parent_index)
        func_index = arena%size
    end function push_function_def

    ! Create subroutine definition node and add to stack
    function push_subroutine_def(arena, name, param_indices, body_indices, &
                                 line, column, parent_index) result(sub_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: sub_index
        type(subroutine_def_node) :: sub_def

        sub_def = create_subroutine_def(name, param_indices, body_indices, line, column)
        call arena%push(sub_def, "subroutine_def", parent_index)
        sub_index = arena%size
    end function push_subroutine_def

    ! Create interface block node and add to stack
    function push_interface_block(arena, interface_name, procedure_indices, &
                                  line, column, parent_index) result(interface_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in), optional :: interface_name
        integer, intent(in), optional :: procedure_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: interface_index
        type(interface_block_node) :: interface_block

        interface_block = create_interface_block(interface_name, "interface", &
                          procedure_indices=procedure_indices, line=line, column=column)
        call arena%push(interface_block, "interface_block", parent_index)
        interface_index = arena%size
    end function push_interface_block

    ! Create module node and add to stack
    function push_module(arena, name, body_indices, line, column, parent_index) result(module_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: module_index
        type(module_node) :: mod_node

        mod_node = create_module(name, declaration_indices=body_indices, line=line, column=column)

        call arena%push(mod_node, "module_node", parent_index)
        module_index = arena%size
    end function push_module

end module ast_factory
