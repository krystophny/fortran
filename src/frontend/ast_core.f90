module ast_core
    use json_module
    use type_system_hm, only: mono_type_t
    use fpm_strings, only: string_t
    implicit none
    private

    ! Base AST node type used by all dialects
    type, abstract, public :: ast_node
        integer :: line = 1
        integer :: column = 1
        type(mono_type_t), allocatable :: inferred_type  ! Type information from semantic analysis
    contains
        procedure(visit_interface), deferred :: accept
        procedure(to_json_interface), deferred :: to_json
    end type ast_node

    ! Stack entry for AST nodes
    type :: ast_entry_t
        class(ast_node), allocatable :: node    ! The AST node itself
        integer :: parent_index = 0             ! Index of parent node in stack (0 for root)
        integer :: depth = 0                    ! Depth in tree (0 for root)
        character(len=:), allocatable :: node_type  ! Type name for debugging
        integer, allocatable :: child_indices(:)    ! Indices of child nodes
        integer :: child_count = 0              ! Number of children
    contains
        procedure :: deep_copy => ast_entry_deep_copy
        procedure :: assign => ast_entry_assign
        generic :: assignment(=) => assign
    end type ast_entry_t

    ! High-performance arena-based AST storage system
    type, public :: ast_arena_t
        type(ast_entry_t), allocatable :: entries(:)  ! Contiguous array of entries
        integer :: size = 0                           ! Current number of entries
        integer :: capacity = 0                       ! Array capacity
        integer :: current_index = 0                  ! Current position in arena
        integer :: max_depth = 0                      ! Maximum depth reached
        integer :: chunk_size = 1024                  ! Default chunk size for growth
        integer :: initial_capacity = 256             ! Starting capacity
    contains
        procedure :: push => ast_arena_push
        procedure :: pop => ast_arena_pop
        procedure :: current => ast_arena_current
        procedure :: get_parent => ast_arena_get_parent
        procedure :: get_depth => ast_arena_get_depth
        procedure :: traverse_depth => ast_arena_traverse_depth
        procedure :: find_by_type => ast_arena_find_by_type
        procedure :: get_children => ast_arena_get_children
        procedure :: get_stats => ast_arena_get_stats
        procedure :: clear => ast_arena_clear
        procedure :: add_child => ast_arena_add_child
        procedure :: shrink_arena
        procedure :: deep_copy => ast_arena_deep_copy
        procedure :: assign => ast_arena_assign
        generic :: assignment(=) => assign
    end type ast_arena_t

    ! Abstract interfaces for visitor pattern and JSON serialization
    abstract interface
        subroutine visit_interface(this, visitor)
            import :: ast_node
            class(ast_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine visit_interface

        subroutine to_json_interface(this, json, parent)
            use json_module
            import :: ast_node
            class(ast_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine to_json_interface
    end interface

    ! Statistics for performance monitoring
    type, public :: ast_arena_stats_t
        integer :: total_nodes = 0
        integer :: max_depth = 0
        integer :: capacity = 0
        integer :: memory_usage = 0  ! Approximate memory usage in bytes
    end type ast_arena_stats_t

    ! Core AST node types shared by all Fortran dialects
    ! KEEP OLD STRUCTURE FOR COMPATIBILITY but also support stack-based access

    ! Program node
    type, extends(ast_node), public :: program_node
        character(len=:), allocatable :: name
        integer, allocatable :: body_indices(:)  ! Indices to body nodes in stack
    contains
        procedure :: accept => program_accept
        procedure :: to_json => program_to_json
    end type program_node

    ! Assignment node
    type, extends(ast_node), public :: assignment_node
        integer :: target_index      ! Index to target node in stack
        integer :: value_index       ! Index to value node in stack
        ! Type inference support (dialect-agnostic)
        logical :: type_was_inferred = .false.  ! true if type was inferred
        character(len=:), allocatable :: inferred_type_name
    contains
        procedure :: accept => assignment_accept
        procedure :: to_json => assignment_to_json
    end type assignment_node

    ! Binary operation node
    type, extends(ast_node), public :: binary_op_node
        integer :: left_index        ! Index to left operand in stack
        integer :: right_index       ! Index to right operand in stack
        character(len=:), allocatable :: operator
    contains
        procedure :: accept => binary_op_accept
        procedure :: to_json => binary_op_to_json
    end type binary_op_node

    ! Function definition node
    type, extends(ast_node), public :: function_def_node
        character(len=:), allocatable :: name
        integer, allocatable :: param_indices(:)
        character(len=:), allocatable :: return_type
        integer, allocatable :: body_indices(:)
    contains
        procedure :: accept => function_def_accept
        procedure :: to_json => function_def_to_json
    end type function_def_node

    ! Subroutine definition node
    type, extends(ast_node), public :: subroutine_def_node
        character(len=:), allocatable :: name
        integer, allocatable :: param_indices(:)
        integer, allocatable :: body_indices(:)
    contains
        procedure :: accept => subroutine_def_accept
        procedure :: to_json => subroutine_def_to_json
    end type subroutine_def_node

    ! Subroutine call node (represents explicit CALL statements)
    type, extends(ast_node), public :: subroutine_call_node
        character(len=:), allocatable :: name
        integer, allocatable :: arg_indices(:)
    contains
        procedure :: accept => subroutine_call_accept
        procedure :: to_json => subroutine_call_to_json
    end type subroutine_call_node

    ! Call or subscript node (represents both function calls and array indexing)
    type, extends(ast_node), public :: call_or_subscript_node
        character(len=:), allocatable :: name
        integer, allocatable :: arg_indices(:)
    contains
        procedure :: accept => call_or_subscript_accept
        procedure :: to_json => call_or_subscript_to_json
    end type call_or_subscript_node

    ! Identifier node
    type, extends(ast_node), public :: identifier_node
        character(len=:), allocatable :: name
    contains
        procedure :: accept => identifier_accept
        procedure :: to_json => identifier_to_json
    end type identifier_node

    ! Literal node
    type, extends(ast_node), public :: literal_node
        character(len=:), allocatable :: value
        integer :: literal_kind = 0  ! INTEGER_LITERAL, REAL_LITERAL, etc.
    contains
        procedure :: accept => literal_accept
        procedure :: to_json => literal_to_json
    end type literal_node

    ! Use statement node
    type, extends(ast_node), public :: use_statement_node
        character(len=:), allocatable :: module_name
        type(string_t), allocatable :: only_list(:)       ! Optional only clause items
        type(string_t), allocatable :: rename_list(:)     ! Optional rename mappings (new_name => old_name)
        logical :: has_only = .false.                     ! Whether the only clause is present
    contains
        procedure :: accept => use_statement_accept
        procedure :: to_json => use_statement_to_json
    end type use_statement_node

    ! Include statement node
    type, extends(ast_node), public :: include_statement_node
        character(len=:), allocatable :: filename
    contains
        procedure :: accept => include_statement_accept
        procedure :: to_json => include_statement_to_json
    end type include_statement_node

    ! Print statement node
    type, extends(ast_node), public :: print_statement_node
        character(len=:), allocatable :: format_spec  ! Optional format
        integer, allocatable :: arg_indices(:)
    contains
        procedure :: accept => print_statement_accept
        procedure :: to_json => print_statement_to_json
    end type print_statement_node

    ! Declaration node
    type, extends(ast_node), public :: declaration_node
        character(len=:), allocatable :: type_name     ! real, integer, etc.
        character(len=:), allocatable :: var_name      ! Variable name
        integer :: kind_value                          ! Kind parameter (e.g., 8 for real(8))
        logical :: has_kind                            ! Whether kind was specified
        character(len=:), allocatable :: intent        ! in, out, inout (for parameters)
        logical :: has_intent = .false.                ! Whether intent was specified
        class(ast_node), allocatable :: initializer    ! Optional initialization value (legacy)
        integer :: initializer_index = 0              ! Initializer index (stack-based)
        logical :: has_initializer = .false.          ! Whether initializer is present
        ! Array dimension support
        logical :: is_array = .false.                  ! Whether this is an array declaration
        type(ast_node_wrapper), allocatable :: dimensions(:) ! Array dimensions (legacy)
        integer, allocatable :: dimension_indices(:)  ! Dimension indices (stack-based)
        logical :: is_allocatable = .false.           ! Whether allocatable attribute is present
    contains
        procedure :: accept => declaration_accept
        procedure :: to_json => declaration_to_json
    end type declaration_node

    ! Parameter declaration node (for function/subroutine parameters)
    type, extends(ast_node), public :: parameter_declaration_node
        character(len=:), allocatable :: name          ! Parameter name
        character(len=:), allocatable :: type_name     ! real, integer, etc.
        integer :: kind_value                          ! Kind parameter (e.g., 8 for real(8))
        logical :: has_kind                            ! Whether kind was specified
        character(len=:), allocatable :: intent        ! in, out, inout
        logical :: has_intent                          ! Whether intent was specified
        ! Array dimension support
        logical :: is_array = .false.                  ! Whether this is an array parameter
        integer, allocatable :: dimension_indices(:)   ! Dimension indices (stack-based)
    contains
        procedure :: accept => parameter_declaration_accept
        procedure :: to_json => parameter_declaration_to_json
    end type parameter_declaration_node

    ! Contains statement node (for separating interface from implementation)
    type, extends(ast_node), public :: contains_node
    contains
        procedure :: accept => contains_accept
        procedure :: to_json => contains_to_json
    end type contains_node

    ! Do loop node
    type, extends(ast_node), public :: do_loop_node
        character(len=:), allocatable :: var_name     ! Loop variable
        integer :: start_expr_index = 0               ! Start expression arena index
        integer :: end_expr_index = 0                 ! End expression arena index
        integer :: step_expr_index = 0                ! Step expression arena index (optional)
        integer, allocatable :: body_indices(:)       ! Loop body arena indices
    contains
        procedure :: accept => do_loop_accept
        procedure :: to_json => do_loop_to_json
    end type do_loop_node

    ! Do while loop node
    type, extends(ast_node), public :: do_while_node
        integer :: condition_index = 0                ! While condition arena index
        integer, allocatable :: body_indices(:)       ! Loop body arena indices
    contains
        procedure :: accept => do_while_accept
        procedure :: to_json => do_while_to_json
    end type do_while_node

    ! If statement node
    type, extends(ast_node), public :: if_node
        integer :: condition_index = 0                ! If condition arena index
        integer, allocatable :: then_body_indices(:) ! Then body arena indices
        type(elseif_wrapper), allocatable :: elseif_blocks(:) ! Elseif blocks (optional)
        integer, allocatable :: else_body_indices(:) ! Else body arena indices (optional)
    contains
        procedure :: accept => if_accept
        procedure :: to_json => if_to_json
    end type if_node

    ! Elseif wrapper (not an AST node itself)
    type, public :: elseif_wrapper
        integer :: condition_index = 0                ! Elseif condition arena index
        integer, allocatable :: body_indices(:)       ! Elseif body arena indices
    end type elseif_wrapper

    ! Derived type definition node
    type, extends(ast_node), public :: derived_type_node
        character(len=:), allocatable :: name          ! Type name
        type(ast_node_wrapper), allocatable :: components(:) ! Type components (legacy)
        integer, allocatable :: component_indices(:)   ! Component indices (stack-based)
        logical :: has_parameters = .false.            ! Whether it has parameters
        type(ast_node_wrapper), allocatable :: parameters(:) ! Type parameters (legacy)
        integer, allocatable :: param_indices(:)       ! Parameter indices (stack-based)
    contains
        procedure :: accept => derived_type_accept
        procedure :: to_json => derived_type_to_json
    end type derived_type_node

    ! Select case node
    type, extends(ast_node), public :: select_case_node
        class(ast_node), allocatable :: expr          ! Expression to match
        type(case_wrapper), allocatable :: cases(:)   ! Case statements
    contains
        procedure :: accept => select_case_accept
        procedure :: to_json => select_case_to_json
    end type select_case_node

    ! Interface block node
    type, extends(ast_node), public :: interface_block_node
        character(len=:), allocatable :: name         ! Interface name (optional)
        character(len=:), allocatable :: kind         ! "interface", "generic", "operator", "assignment"
        character(len=:), allocatable :: operator     ! Operator symbol (for operator interfaces)
        integer, allocatable :: procedure_indices(:)  ! Procedure declaration arena indices
    contains
        procedure :: accept => interface_block_accept
        procedure :: to_json => interface_block_to_json
    end type interface_block_node

    ! Module node
    type, extends(ast_node), public :: module_node
        character(len=:), allocatable :: name         ! Module name
        integer, allocatable :: declaration_indices(:) ! Module declaration arena indices
        integer, allocatable :: procedure_indices(:)   ! Module procedure arena indices (after contains)
        logical :: has_contains = .false.             ! Whether module has a contains section
    contains
        procedure :: accept => module_accept
        procedure :: to_json => module_to_json
    end type module_node

    ! Case statement wrapper
    type, public :: case_wrapper
        character(len=:), allocatable :: case_type    ! "case", "case_default"
        class(ast_node), allocatable :: value         ! Case value (optional for default)
        type(ast_node_wrapper), allocatable :: body(:) ! Case body
    end type case_wrapper

    ! Wrapper type for polymorphic arrays - BUT NOW BACKED BY STACK
    type, public :: ast_node_wrapper
        class(ast_node), allocatable :: node
        integer :: stack_index = 0  ! NEW: Index in AST stack for O(depth) access
    end type ast_node_wrapper

    ! Literal kind constants
    integer, parameter, public :: LITERAL_INTEGER = 1
    integer, parameter, public :: LITERAL_REAL = 2
    integer, parameter, public :: LITERAL_STRING = 3
    integer, parameter, public :: LITERAL_LOGICAL = 4

    ! Public interface for creating nodes and stack
    public :: create_ast_stack
    public :: create_program, create_assignment, create_binary_op
    public :: create_function_def, create_subroutine_def, create_call_or_subscript, create_subroutine_call
    public :: create_identifier, create_literal, create_use_statement, create_include_statement, create_print_statement
    public :: create_declaration, create_do_loop, create_do_while, create_if, create_select_case
    public :: create_derived_type, create_interface_block, create_module

contains

    ! Create a new high-performance AST arena
    function create_ast_stack(initial_capacity) result(arena)
        integer, intent(in), optional :: initial_capacity
        type(ast_arena_t) :: arena
        integer :: cap

        ! Use chunk-aligned initial capacity for optimal performance
        if (present(initial_capacity)) then
            cap = max(initial_capacity, arena%initial_capacity)
        else
            cap = arena%initial_capacity
        end if

        arena%capacity = cap
        arena%chunk_size = 1024  ! High-performance chunk size
        arena%initial_capacity = 256
        allocate (arena%entries(cap))
        arena%size = 0
        arena%current_index = 0
        arena%max_depth = 0
    end function create_ast_stack

    ! Push a new AST node onto the stack
    subroutine ast_arena_push(this, node, node_type, parent_index)
        class(ast_arena_t), intent(inout) :: this
        class(ast_node), intent(in) :: node
        character(len=*), intent(in), optional :: node_type
        integer, intent(in), optional :: parent_index
        type(ast_entry_t), allocatable :: temp_entries(:)
        integer :: new_capacity, parent_depth, parent_idx

        ! Grow array using buffered chunk allocation for high performance
        if (this%size >= this%capacity) then
            if (this%capacity == 0) then
                new_capacity = this%initial_capacity
            else
                ! Grow by chunk_size to minimize allocations
                new_capacity = this%capacity + this%chunk_size
            end if
            allocate (temp_entries(new_capacity))
            if (this%size > 0) then
                temp_entries(1:this%size) = this%entries(1:this%size)
            end if
            this%entries = temp_entries
            this%capacity = new_capacity
        end if

        ! Add new entry
        this%size = this%size + 1
        this%current_index = this%size

        ! Store the node using allocatable source
        allocate (this%entries(this%size)%node, source=node)

        ! Set parent index and depth
        if (present(parent_index)) then
            parent_idx = parent_index
        else
            parent_idx = 0  ! Root node
        end if

        this%entries(this%size)%parent_index = parent_idx

        if (parent_idx == 0) then
            this%entries(this%size)%depth = 0
        else
            parent_depth = this%entries(parent_idx)%depth
            this%entries(this%size)%depth = parent_depth + 1
        end if

        ! Update max depth
        if (this%entries(this%size)%depth > this%max_depth) then
            this%max_depth = this%entries(this%size)%depth
        end if

        ! Store type name for debugging
        if (present(node_type)) then
            this%entries(this%size)%node_type = node_type
        else
            this%entries(this%size)%node_type = "unknown"
        end if

        ! Initialize child array
        this%entries(this%size)%child_count = 0

        ! Add to parent's child list
        if (parent_idx > 0) then
            call this%add_child(parent_idx, this%size)
        end if
    end subroutine ast_arena_push

    ! Add a child to a parent node
    subroutine ast_arena_add_child(this, parent_index, child_index)
        class(ast_arena_t), intent(inout) :: this
        integer, intent(in) :: parent_index, child_index
        integer, allocatable :: temp_children(:)
        integer :: new_size

        if (parent_index <= 0 .or. parent_index > this%size) return
        if (child_index <= 0 .or. child_index > this%size) return

        ! Grow child array if needed
        if (.not. allocated(this%entries(parent_index)%child_indices)) then
            allocate (this%entries(parent_index)%child_indices(10))
        else if (this%entries(parent_index)%child_count >= size(this%entries(parent_index)%child_indices)) then
            new_size = size(this%entries(parent_index)%child_indices)*2
            allocate (temp_children(new_size))
            temp_children(1:this%entries(parent_index)%child_count) = &
      this%entries(parent_index)%child_indices(1:this%entries(parent_index)%child_count)
            this%entries(parent_index)%child_indices = temp_children
        end if

        ! Add child
     this%entries(parent_index)%child_count = this%entries(parent_index)%child_count + 1
        this%entries(parent_index)%child_indices(this%entries(parent_index)%child_count) = child_index
    end subroutine ast_arena_add_child

    ! Pop the current node from arena (with memory cleanup)
    subroutine ast_arena_pop(this)
        class(ast_arena_t), intent(inout) :: this

        if (this%size > 0) then
            ! Clean up the node allocation (ordered release)
            if (allocated(this%entries(this%size)%node)) then
                deallocate (this%entries(this%size)%node)
            end if
            if (allocated(this%entries(this%size)%child_indices)) then
                deallocate (this%entries(this%size)%child_indices)
            end if

            this%size = this%size - 1
            if (this%size > 0) then
                this%current_index = this%size
            else
                this%current_index = 0
            end if

            ! Shrink arena if using less than 25% of capacity and over chunk size
            if (this%capacity > this%chunk_size .and. &
                this%size < this%capacity/4) then
                call this%shrink_arena()
            end if
        else
            error stop "Cannot pop from empty AST arena"
        end if
    end subroutine ast_arena_pop

    ! Get the current node from the stack
    function ast_arena_current(this) result(node)
        class(ast_arena_t), intent(in) :: this
        class(ast_node), allocatable :: node

        if (this%current_index > 0 .and. this%current_index <= this%size) then
            if (allocated(this%entries(this%current_index)%node)) then
                allocate (node, source=this%entries(this%current_index)%node)
            end if
        end if
    end function ast_arena_current

    ! Get the parent of the current node
    function ast_arena_get_parent(this, index) result(parent_node)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in), optional :: index
        class(ast_node), allocatable :: parent_node
        integer :: idx, parent_idx

        idx = this%current_index
        if (present(index)) idx = index

        if (idx > 0 .and. idx <= this%size) then
            parent_idx = this%entries(idx)%parent_index
            if (parent_idx > 0) then
                allocate (parent_node, source=this%entries(parent_idx)%node)
            end if
        end if
    end function ast_arena_get_parent

    ! Get the depth of a node (or current node)
    function ast_arena_get_depth(this, index) result(depth)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in), optional :: index
        integer :: depth, idx

        idx = this%current_index
        if (present(index)) idx = index

        depth = -1  ! Invalid depth
        if (idx > 0 .and. idx <= this%size) then
            depth = this%entries(idx)%depth
        end if
    end function ast_arena_get_depth

    ! Traverse nodes at a specific depth (O(depth) complexity)
    subroutine ast_arena_traverse_depth(this, target_depth, visitor)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: target_depth
        class(*), intent(inout) :: visitor
        integer :: i

        ! Linear scan through stack entries (cache-efficient)
        do i = 1, this%size
            if (this%entries(i)%depth == target_depth) then
                if (allocated(this%entries(i)%node)) then
                    call this%entries(i)%node%accept(visitor)
                end if
            end if
        end do
    end subroutine ast_arena_traverse_depth

    ! Find nodes by type (O(n) but cache-efficient)
    function ast_arena_find_by_type(this, node_type) result(indices)
        class(ast_arena_t), intent(in) :: this
        character(len=*), intent(in) :: node_type
        integer, allocatable :: indices(:)
        integer, allocatable :: temp_indices(:)
        integer :: i, count

        ! Count matching nodes
        count = 0
        do i = 1, this%size
            if (allocated(this%entries(i)%node_type)) then
                if (this%entries(i)%node_type == node_type) then
                    count = count + 1
                end if
            end if
        end do

        ! Allocate result array
        if (count > 0) then
            allocate (temp_indices(count))
            count = 0
            do i = 1, this%size
                if (allocated(this%entries(i)%node_type)) then
                    if (this%entries(i)%node_type == node_type) then
                        count = count + 1
                        temp_indices(count) = i
                    end if
                end if
            end do
            indices = temp_indices
        else
            allocate (indices(0))
        end if
    end function ast_arena_find_by_type

    ! Get children of a node (O(1) lookup)
    function ast_arena_get_children(this, parent_index) result(child_indices)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: parent_index
        integer, allocatable :: child_indices(:)

        if (parent_index > 0 .and. parent_index <= this%size) then
            if (allocated(this%entries(parent_index)%child_indices)) then
                allocate (child_indices(this%entries(parent_index)%child_count))
                child_indices = this%entries(parent_index)%child_indices(1:this%entries(parent_index)%child_count)
            else
                allocate (child_indices(0))
            end if
        else
            allocate (child_indices(0))
        end if
    end function ast_arena_get_children

    ! Get performance statistics
    function ast_arena_get_stats(this) result(stats)
        class(ast_arena_t), intent(in) :: this
        type(ast_arena_stats_t) :: stats

        stats%total_nodes = this%size
        stats%max_depth = this%max_depth
        stats%capacity = this%capacity
        stats%memory_usage = this%capacity*100  ! Rough estimate in bytes
    end function ast_arena_get_stats

    ! Clear the stack
    subroutine ast_arena_clear(this)
        class(ast_arena_t), intent(inout) :: this
        integer :: i

        ! Clean up all node allocations in reverse order (ordered release)
        do i = this%size, 1, -1
            if (allocated(this%entries(i)%node)) then
                deallocate (this%entries(i)%node)
            end if
            if (allocated(this%entries(i)%child_indices)) then
                deallocate (this%entries(i)%child_indices)
            end if
        end do

        this%size = 0
        this%current_index = 0
        this%max_depth = 0

        ! Reset to initial capacity to prevent memory leaks
        if (allocated(this%entries)) then
            deallocate (this%entries)
            allocate (this%entries(this%initial_capacity))
            this%capacity = this%initial_capacity
        end if
    end subroutine ast_arena_clear

    ! Factory functions for creating AST nodes (KEEP ALL ORIGINAL SIGNATURES)

    function create_program(name, body_indices, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in) :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(program_node) :: node
        integer :: i

        node%name = name
        if (size(body_indices) > 0) then
            allocate (node%body_indices(size(body_indices)))
            do i = 1, size(body_indices)
                node%body_indices(i) = body_indices(i)
            end do
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_program

    function create_assignment(target_index, value_index, line, column, inferred_type, inferred_type_name) result(node)
        integer, intent(in) :: target_index
        integer, intent(in) :: value_index
        integer, intent(in), optional :: line, column
        logical, intent(in), optional :: inferred_type
        character(len=*), intent(in), optional :: inferred_type_name
        type(assignment_node) :: node

        node%target_index = target_index
        node%value_index = value_index
        if (present(line)) node%line = line
        if (present(column)) node%column = column
        if (present(inferred_type)) node%type_was_inferred = inferred_type
        if (present(inferred_type_name)) node%inferred_type_name = inferred_type_name
    end function create_assignment

 function create_binary_op(left_index, right_index, operator, line, column) result(node)
        integer, intent(in) :: left_index
        integer, intent(in) :: right_index
        character(len=*), intent(in) :: operator
        integer, intent(in), optional :: line, column
        type(binary_op_node) :: node

        node%left_index = left_index
        node%right_index = right_index
        node%operator = operator
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_binary_op

function create_function_def(name, param_indices, return_type, body_indices, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in) :: param_indices(:)
        character(len=*), intent(in) :: return_type
        integer, intent(in) :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(function_def_node) :: node

        node%name = name
        if (size(param_indices) > 0) then
            node%param_indices = param_indices
        end if
        node%return_type = return_type
        if (size(body_indices) > 0) then
            node%body_indices = body_indices
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_function_def

    function create_subroutine_def(name, param_indices, body_indices, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in) :: param_indices(:)
        integer, intent(in) :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(subroutine_def_node) :: node

        node%name = name
        if (size(param_indices) > 0) then
            node%param_indices = param_indices
        end if
        if (size(body_indices) > 0) then
            node%body_indices = body_indices
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_subroutine_def

    function create_subroutine_call(name, args, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in) :: args(:)
        integer, intent(in), optional :: line, column
        type(subroutine_call_node) :: node
        integer :: i

        node%name = name
        if (size(args) > 0) then
            allocate (node%arg_indices(size(args)))
            do i = 1, size(args)
                node%arg_indices(i) = args(i)
            end do
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_subroutine_call

    function create_call_or_subscript(name, args, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in) :: args(:)
        integer, intent(in), optional :: line, column
        type(call_or_subscript_node) :: node
        integer :: i

        node%name = name
        if (size(args) > 0) then
            allocate (node%arg_indices(size(args)))
            do i = 1, size(args)
                node%arg_indices(i) = args(i)
            end do
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_call_or_subscript

    function create_identifier(name, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: line, column
        type(identifier_node) :: node

        node%name = name
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_identifier

    function create_literal(value, kind, line, column) result(node)
        character(len=*), intent(in) :: value
        integer, intent(in) :: kind
        integer, intent(in), optional :: line, column
        type(literal_node) :: node

        node%value = value
        node%literal_kind = kind
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_literal

    function create_declaration(type_name, var_name, kind_value, initializer, dimensions, is_allocatable, line, column) result(node)
        character(len=*), intent(in) :: type_name
        character(len=*), intent(in) :: var_name
        integer, intent(in), optional :: kind_value
        class(ast_node), allocatable, intent(in), optional :: initializer
        type(ast_node_wrapper), intent(in), optional :: dimensions(:)
        logical, intent(in), optional :: is_allocatable
        integer, intent(in), optional :: line, column
        type(declaration_node) :: node

        node%type_name = type_name
        node%var_name = var_name

        if (present(kind_value)) then
            node%kind_value = kind_value
            node%has_kind = .true.
        else
            node%kind_value = 0
            node%has_kind = .false.
        end if

        if (present(initializer)) then
            allocate (node%initializer, source=initializer)
        end if

        if (present(dimensions)) then
            node%is_array = .true.
            allocate (node%dimensions, source=dimensions)
        else
            node%is_array = .false.
        end if

        if (present(is_allocatable)) then
            node%is_allocatable = is_allocatable
        else
            node%is_allocatable = .false.
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_declaration

    function create_parameter_declaration(name, type_name, kind_value, intent, dimension_indices, line, column) result(node)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: type_name
        integer, intent(in), optional :: kind_value
        character(len=*), intent(in), optional :: intent
        integer, intent(in), optional :: dimension_indices(:)
        integer, intent(in), optional :: line, column
        type(parameter_declaration_node) :: node

        node%name = name
        node%type_name = type_name

        if (present(kind_value)) then
            node%kind_value = kind_value
            node%has_kind = .true.
        else
            node%kind_value = 0
            node%has_kind = .false.
        end if

        if (present(intent)) then
            node%intent = intent
            node%has_intent = .true.
        else
            node%intent = "in"  ! Default to intent(in)
            node%has_intent = .false.
        end if

        if (present(dimension_indices) .and. size(dimension_indices) > 0) then
            node%is_array = .true.
            node%dimension_indices = dimension_indices
        else
            node%is_array = .false.
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_parameter_declaration

    function create_use_statement(module_name, only_list, rename_list, has_only, line, column) result(node)
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in), optional :: only_list(:)
        character(len=*), intent(in), optional :: rename_list(:)
        logical, intent(in), optional :: has_only
        integer, intent(in), optional :: line, column
        type(use_statement_node) :: node

        node%module_name = module_name
        if (present(only_list)) then
            allocate (node%only_list(size(only_list)))
            block
                integer :: i
                do i = 1, size(only_list)
                    node%only_list(i)%s = trim(only_list(i))
                end do
            end block
        end if
        if (present(rename_list)) then
            allocate (node%rename_list(size(rename_list)))
            block
                integer :: i
                do i = 1, size(rename_list)
                    node%rename_list(i)%s = trim(rename_list(i))
                end do
            end block
        end if
        if (present(has_only)) then
            node%has_only = has_only
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_use_statement

    function create_include_statement(filename, line, column) result(node)
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: line, column
        type(include_statement_node) :: node

        node%filename = filename
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_include_statement

    function create_print_statement(arg_indices, format_spec, line, column) result(node)
        integer, intent(in) :: arg_indices(:)
        character(len=*), intent(in), optional :: format_spec
        integer, intent(in), optional :: line, column
        type(print_statement_node) :: node

        if (size(arg_indices) > 0) then
            node%arg_indices = arg_indices
        end if
        if (present(format_spec)) node%format_spec = format_spec
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_print_statement

    function create_do_loop(var_name, start_expr_index, end_expr_index, step_expr_index, body_indices, line, column) result(node)
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_expr_index, end_expr_index
        integer, intent(in), optional :: step_expr_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(do_loop_node) :: node

        node%var_name = var_name
        node%start_expr_index = start_expr_index
        node%end_expr_index = end_expr_index
        if (present(step_expr_index)) node%step_expr_index = step_expr_index

        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_do_loop

    function create_do_while(condition_index, body_indices, line, column) result(node)
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(do_while_node) :: node

        node%condition_index = condition_index

        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_do_while

    function create_if(condition_index, then_body_indices, elseif_blocks, else_body_indices, line, column) result(node)
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: then_body_indices(:)
        type(elseif_wrapper), intent(in), optional :: elseif_blocks(:)
        integer, intent(in), optional :: else_body_indices(:)
        integer, intent(in), optional :: line, column
        type(if_node) :: node

        node%condition_index = condition_index

        if (present(then_body_indices) .and. size(then_body_indices) > 0) then
            node%then_body_indices = then_body_indices
        end if

        if (present(elseif_blocks) .and. size(elseif_blocks) > 0) then
            allocate (node%elseif_blocks, source=elseif_blocks)
        end if

        if (present(else_body_indices) .and. size(else_body_indices) > 0) then
            node%else_body_indices = else_body_indices
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_if

    function create_select_case(expr, cases, line, column) result(node)
        class(ast_node), intent(in) :: expr
        type(case_wrapper), intent(in), optional :: cases(:)
        integer, intent(in), optional :: line, column
        type(select_case_node) :: node

        allocate (node%expr, source=expr)

        if (present(cases) .and. size(cases) > 0) then
            allocate (node%cases(size(cases)))
            node%cases = cases
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_select_case

   function create_derived_type(name, components, parameters, line, column) result(node)
        character(len=*), intent(in) :: name
        type(ast_node_wrapper), intent(in), optional :: components(:)
        type(ast_node_wrapper), intent(in), optional :: parameters(:)
        integer, intent(in), optional :: line, column
        type(derived_type_node) :: node

        node%name = name

        if (present(components)) then
            if (size(components) > 0) then
                allocate (node%components, source=components)
            end if
        end if

        if (present(parameters)) then
            if (size(parameters) > 0) then
                node%has_parameters = .true.
                allocate (node%parameters, source=parameters)
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_derived_type

    function create_interface_block(name, kind, operator, procedure_indices, line, column) result(node)
        character(len=*), intent(in), optional :: name
        character(len=*), intent(in) :: kind
        character(len=*), intent(in), optional :: operator
        integer, intent(in), optional :: procedure_indices(:)
        integer, intent(in), optional :: line, column
        type(interface_block_node) :: node

        if (present(name)) then
            node%name = name
        end if

        node%kind = kind

        if (present(operator)) then
            node%operator = operator
        end if

        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                node%procedure_indices = procedure_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_interface_block

    function create_module(name, declaration_indices, procedure_indices, has_contains, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: declaration_indices(:)
        integer, intent(in), optional :: procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column
        type(module_node) :: node

        node%name = name

        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) then
                node%declaration_indices = declaration_indices
            end if
        end if

        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                node%procedure_indices = procedure_indices
            end if
        end if

        if (present(has_contains)) then
            node%has_contains = has_contains
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_module

    ! JSON serialization implementations (I'll include the key ones)

    subroutine program_to_json(this, json, parent)
        class(program_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, body_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'program')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(body_array, 'body')
        call json%add(obj, body_array)

        if (allocated(this%body_indices)) then
            do i = 1, size(this%body_indices)
                block
                    type(json_value), pointer :: body_obj
                    call json%create_object(body_obj, '')
                    call json%add(body_obj, 'stack_index', this%body_indices(i))
                    call json%add(body_array, body_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine program_to_json

    subroutine assignment_to_json(this, json, parent)
        class(assignment_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'assignment')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'inferred_type', this%type_was_inferred)
        if (allocated(this%inferred_type_name)) then
            call json%add(obj, 'inferred_type_name', this%inferred_type_name)
        end if

        ! Add target and value as stack indices
        call json%add(obj, 'target_index', this%target_index)
        call json%add(obj, 'value_index', this%value_index)

        call json%add(parent, obj)
    end subroutine assignment_to_json

    subroutine binary_op_to_json(this, json, parent)
        class(binary_op_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'binary_op')
        call json%add(obj, 'operator', this%operator)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        ! Add left and right as stack indices
        call json%add(obj, 'left_index', this%left_index)
        call json%add(obj, 'right_index', this%right_index)

        call json%add(parent, obj)
    end subroutine binary_op_to_json

    subroutine function_def_to_json(this, json, parent)
        class(function_def_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, params_array, body_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'function_def')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(params_array, 'param_indices')
        call json%add(obj, params_array)
        if (allocated(this%param_indices)) then
            do i = 1, size(this%param_indices)
                block
                    type(json_value), pointer :: param_obj
                    call json%create_object(param_obj, '')
                    call json%add(param_obj, 'stack_index', this%param_indices(i))
                    call json%add(params_array, param_obj)
                end block
            end do
        end if

        call json%add(obj, 'return_type', this%return_type)

        call json%create_array(body_array, 'body_indices')
        call json%add(obj, body_array)
        if (allocated(this%body_indices)) then
            do i = 1, size(this%body_indices)
                block
                    type(json_value), pointer :: body_obj
                    call json%create_object(body_obj, '')
                    call json%add(body_obj, 'stack_index', this%body_indices(i))
                    call json%add(body_array, body_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine function_def_to_json

    subroutine subroutine_def_to_json(this, json, parent)
        class(subroutine_def_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, params_array, body_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'subroutine_def')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(params_array, 'param_indices')
        call json%add(obj, params_array)
        if (allocated(this%param_indices)) then
            do i = 1, size(this%param_indices)
                block
                    type(json_value), pointer :: param_obj
                    call json%create_object(param_obj, '')
                    call json%add(param_obj, 'stack_index', this%param_indices(i))
                    call json%add(params_array, param_obj)
                end block
            end do
        end if

        call json%create_array(body_array, 'body_indices')
        call json%add(obj, body_array)
        if (allocated(this%body_indices)) then
            do i = 1, size(this%body_indices)
                block
                    type(json_value), pointer :: body_obj
                    call json%create_object(body_obj, '')
                    call json%add(body_obj, 'stack_index', this%body_indices(i))
                    call json%add(body_array, body_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine subroutine_def_to_json

    subroutine subroutine_call_to_json(this, json, parent)
        class(subroutine_call_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, args_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'subroutine_call')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(args_array, 'args')
        call json%add(obj, args_array)
        if (allocated(this%arg_indices)) then
            do i = 1, size(this%arg_indices)
                block
                    type(json_value), pointer :: arg_obj
                    call json%create_object(arg_obj, '')
                    call json%add(arg_obj, 'stack_index', this%arg_indices(i))
                    call json%add(args_array, arg_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine subroutine_call_to_json

    subroutine call_or_subscript_to_json(this, json, parent)
        class(call_or_subscript_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, args_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'call_or_subscript')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(args_array, 'args')
        call json%add(obj, args_array)
        if (allocated(this%arg_indices)) then
            do i = 1, size(this%arg_indices)
                block
                    type(json_value), pointer :: arg_obj
                    call json%create_object(arg_obj, '')
                    call json%add(arg_obj, 'stack_index', this%arg_indices(i))
                    call json%add(args_array, arg_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine call_or_subscript_to_json

    subroutine identifier_to_json(this, json, parent)
        class(identifier_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'identifier')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%add(parent, obj)
    end subroutine identifier_to_json

    subroutine literal_to_json(this, json, parent)
        class(literal_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        character(len=:), allocatable :: kind_name

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'literal')
        call json%add(obj, 'value', this%value)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        select case (this%literal_kind)
        case (LITERAL_INTEGER)
            kind_name = 'integer'
        case (LITERAL_REAL)
            kind_name = 'real'
        case (LITERAL_STRING)
            kind_name = 'string'
        case (LITERAL_LOGICAL)
            kind_name = 'logical'
        case default
            kind_name = 'unknown'
        end select
        call json%add(obj, 'kind', kind_name)

        call json%add(parent, obj)
    end subroutine literal_to_json

    subroutine use_statement_to_json(this, json, parent)
        class(use_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, only_array, rename_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'use_statement')
        call json%add(obj, 'module_name', this%module_name)
        call json%add(obj, 'has_only', this%has_only)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (allocated(this%only_list)) then
            call json%create_array(only_array, 'only_list')
            call json%add(obj, only_array)
            do i = 1, size(this%only_list)
                if (allocated(this%only_list(i)%s)) then
                    call json%add(only_array, '', this%only_list(i)%s)
                else
                    call json%add(only_array, '', '')
                end if
            end do
        end if

        if (allocated(this%rename_list)) then
            call json%create_array(rename_array, 'rename_list')
            call json%add(obj, rename_array)
            do i = 1, size(this%rename_list)
                if (allocated(this%rename_list(i)%s)) then
                    call json%add(rename_array, '', this%rename_list(i)%s)
                else
                    call json%add(rename_array, '', '')
                end if
            end do
        end if

        call json%add(parent, obj)
    end subroutine use_statement_to_json

    subroutine include_statement_to_json(this, json, parent)
        class(include_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'include_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'filename', this%filename)

        call json%add(parent, obj)
    end subroutine include_statement_to_json

    subroutine print_statement_to_json(this, json, parent)
        class(print_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, args_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'print_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (allocated(this%format_spec)) then
            call json%add(obj, 'format_spec', this%format_spec)
        end if

        call json%create_array(args_array, 'arg_indices')
        call json%add(obj, args_array)
        if (allocated(this%arg_indices)) then
            do i = 1, size(this%arg_indices)
                block
                    type(json_value), pointer :: arg_obj
                    call json%create_object(arg_obj, '')
                    call json%add(arg_obj, 'stack_index', this%arg_indices(i))
                    call json%add(args_array, arg_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine print_statement_to_json

    subroutine declaration_to_json(this, json, parent)
        class(declaration_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'declaration')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'type_name', this%type_name)
        call json%add(obj, 'var_name', this%var_name)

        if (this%has_kind) then
            call json%add(obj, 'kind_value', this%kind_value)
        end if

        if (this%has_intent) then
            call json%add(obj, 'intent', this%intent)
        end if

        if (this%is_array) then
            call json%add(obj, 'is_array', .true.)
            if (this%is_allocatable) then
                call json%add(obj, 'is_allocatable', .true.)
            end if

            if (allocated(this%dimensions)) then
                block
                    type(json_value), pointer :: dims_array
                    call json%create_array(dims_array, 'dimensions')
                    do i = 1, size(this%dimensions)
                        if (allocated(this%dimensions(i)%node)) then
                            call this%dimensions(i)%node%to_json(json, dims_array)
                        end if
                    end do
                    call json%add(obj, dims_array)
                end block
            end if
        end if

        if (allocated(this%initializer)) then
            block
                type(json_value), pointer :: init_obj
                call json%create_object(init_obj, 'initializer')
                call this%initializer%to_json(json, init_obj)
                call json%add(obj, init_obj)
            end block
        end if

        call json%add(parent, obj)
    end subroutine declaration_to_json

    ! Parameter declaration node to JSON
    subroutine parameter_declaration_to_json(this, json, parent)
        class(parameter_declaration_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'parameter_declaration')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'type_name', this%type_name)

        if (this%has_kind) then
            call json%add(obj, 'kind_value', this%kind_value)
        end if

        if (this%has_intent) then
            call json%add(obj, 'intent', this%intent)
        end if

        if (this%is_array) then
            call json%add(obj, 'is_array', .true.)
            if (allocated(this%dimension_indices)) then
                call json%add(obj, 'dimension_indices', this%dimension_indices)
            end if
        end if

        call json%add(parent, obj)
    end subroutine parameter_declaration_to_json

    ! Contains node accept visitor
    subroutine contains_accept(this, visitor)
        class(contains_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! No children to visit
    end subroutine contains_accept

    ! Contains node to JSON
    subroutine contains_to_json(this, json, parent)
        class(contains_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'node_type', 'contains')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%add(parent, obj)
    end subroutine contains_to_json

    subroutine do_loop_to_json(this, json, parent)
        class(do_loop_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'do_loop')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'var_name', this%var_name)
        call json%add(parent, obj)
    end subroutine do_loop_to_json

    subroutine do_while_to_json(this, json, parent)
        class(do_while_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'do_while')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine do_while_to_json

    subroutine if_to_json(this, json, parent)
        class(if_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
      type(json_value), pointer :: obj, then_array, else_array, elseif_array, elseif_obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'if_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        ! Add condition index
        call json%add(obj, 'condition_index', this%condition_index)

        ! Add then body indices
        if (allocated(this%then_body_indices)) then
            call json%create_array(then_array, 'then_body_indices')
            call json%add(obj, then_array)
            do i = 1, size(this%then_body_indices)
                block
                    type(json_value), pointer :: stmt_obj
                    call json%create_object(stmt_obj, '')
                    call json%add(stmt_obj, 'stack_index', this%then_body_indices(i))
                    call json%add(then_array, stmt_obj)
                end block
            end do
        end if

        ! Add elseif blocks (simplified arena-based)
        if (allocated(this%elseif_blocks)) then
            call json%create_array(elseif_array, 'elseif_blocks')
            call json%add(obj, elseif_array)
            do i = 1, size(this%elseif_blocks)
                call json%create_object(elseif_obj, '')
     call json%add(elseif_obj, 'condition_index', this%elseif_blocks(i)%condition_index)
                if (allocated(this%elseif_blocks(i)%body_indices)) then
                    block
                        type(json_value), pointer :: body_array
                        integer :: j
                        call json%create_array(body_array, 'body_indices')
                        call json%add(elseif_obj, body_array)
                        do j = 1, size(this%elseif_blocks(i)%body_indices)
                            block
                                type(json_value), pointer :: stmt_obj
                                call json%create_object(stmt_obj, '')
           call json%add(stmt_obj, 'stack_index', this%elseif_blocks(i)%body_indices(j))
                                call json%add(body_array, stmt_obj)
                            end block
                        end do
                    end block
                end if
                call json%add(elseif_array, elseif_obj)
            end do
        end if

        ! Add else body indices
        if (allocated(this%else_body_indices)) then
            call json%create_array(else_array, 'else_body_indices')
            call json%add(obj, else_array)
            do i = 1, size(this%else_body_indices)
                block
                    type(json_value), pointer :: stmt_obj
                    call json%create_object(stmt_obj, '')
                    call json%add(stmt_obj, 'stack_index', this%else_body_indices(i))
                    call json%add(else_array, stmt_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine if_to_json

    subroutine select_case_to_json(this, json, parent)
        class(select_case_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'select_case')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine select_case_to_json

    subroutine derived_type_to_json(this, json, parent)
        class(derived_type_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'derived_type')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (this%has_parameters) then
            call json%add(obj, 'has_parameters', .true.)
            if (allocated(this%parameters)) then
                block
                    type(json_value), pointer :: params_array
                    call json%create_array(params_array, 'parameters')
                    do i = 1, size(this%parameters)
                        if (allocated(this%parameters(i)%node)) then
                            call this%parameters(i)%node%to_json(json, params_array)
                        end if
                    end do
                    call json%add(obj, params_array)
                end block
            end if
        end if

        if (allocated(this%components)) then
            block
                type(json_value), pointer :: components_array
                call json%create_array(components_array, 'components')
                do i = 1, size(this%components)
                    if (allocated(this%components(i)%node)) then
                        call this%components(i)%node%to_json(json, components_array)
                    end if
                end do
                call json%add(obj, components_array)
            end block
        end if

        call json%add(parent, obj)
    end subroutine derived_type_to_json

    subroutine interface_block_to_json(this, json, parent)
        class(interface_block_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'interface_block')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'kind', this%kind)

        if (allocated(this%name)) then
            call json%add(obj, 'name', this%name)
        end if

        if (allocated(this%operator)) then
            call json%add(obj, 'operator', this%operator)
        end if

        if (allocated(this%procedure_indices)) then
            block
                type(json_value), pointer :: procedures_array
                call json%create_array(procedures_array, 'procedure_indices')
                do i = 1, size(this%procedure_indices)
                    block
                        type(json_value), pointer :: proc_obj
                        call json%create_object(proc_obj, '')
                       call json%add(proc_obj, 'arena_index', this%procedure_indices(i))
                        call json%add(procedures_array, proc_obj)
                    end block
                end do
                call json%add(obj, procedures_array)
            end block
        end if

        call json%add(parent, obj)
    end subroutine interface_block_to_json

    subroutine module_to_json(this, json, parent)
        class(module_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'node_type', 'module')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'has_contains', this%has_contains)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        ! Add declaration indices array
        if (allocated(this%declaration_indices)) then
            block
                type(json_value), pointer :: declarations_array
                call json%create_array(declarations_array, 'declaration_indices')
                do i = 1, size(this%declaration_indices)
                    block
                        type(json_value), pointer :: decl_obj
                        call json%create_object(decl_obj, '')
                     call json%add(decl_obj, 'arena_index', this%declaration_indices(i))
                        call json%add(declarations_array, decl_obj)
                    end block
                end do
                call json%add(obj, declarations_array)
            end block
        end if

        ! Add procedure indices array
        if (allocated(this%procedure_indices)) then
            block
                type(json_value), pointer :: procedures_array
                call json%create_array(procedures_array, 'procedure_indices')
                do i = 1, size(this%procedure_indices)
                    block
                        type(json_value), pointer :: proc_obj
                        call json%create_object(proc_obj, '')
                       call json%add(proc_obj, 'arena_index', this%procedure_indices(i))
                        call json%add(procedures_array, proc_obj)
                    end block
                end do
                call json%add(obj, procedures_array)
            end block
        end if

        call json%add(parent, obj)
    end subroutine module_to_json

    ! High-performance arena memory management operations

    ! Shrink arena to optimal size (chunk-based memory management)
    subroutine shrink_arena(this)
        class(ast_arena_t), intent(inout) :: this
        type(ast_entry_t), allocatable :: temp_entries(:)
        integer :: new_capacity

        ! Calculate optimal new capacity (multiple of chunk_size)
        new_capacity = max(this%initial_capacity, &
                           ((this%size/this%chunk_size) + 1)*this%chunk_size)

        if (new_capacity < this%capacity) then
            allocate (temp_entries(new_capacity))
            if (this%size > 0) then
                temp_entries(1:this%size) = this%entries(1:this%size)
            end if
            this%entries = temp_entries
            this%capacity = new_capacity
        end if
    end subroutine shrink_arena

    ! Visitor pattern implementations (basic versions for core types)
    subroutine program_accept(this, visitor)
        class(program_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine program_accept

    subroutine assignment_accept(this, visitor)
        class(assignment_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine assignment_accept

    subroutine binary_op_accept(this, visitor)
        class(binary_op_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine binary_op_accept

    subroutine function_def_accept(this, visitor)
        class(function_def_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine function_def_accept

    subroutine subroutine_def_accept(this, visitor)
        class(subroutine_def_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine subroutine_def_accept

    subroutine subroutine_call_accept(this, visitor)
        class(subroutine_call_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine subroutine_call_accept

    subroutine call_or_subscript_accept(this, visitor)
        class(call_or_subscript_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine call_or_subscript_accept

    subroutine identifier_accept(this, visitor)
        class(identifier_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine identifier_accept

    subroutine literal_accept(this, visitor)
        class(literal_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine literal_accept

    subroutine use_statement_accept(this, visitor)
        class(use_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine use_statement_accept

    subroutine include_statement_accept(this, visitor)
        class(include_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine include_statement_accept

    subroutine print_statement_accept(this, visitor)
        class(print_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine print_statement_accept

    subroutine declaration_accept(this, visitor)
        class(declaration_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine declaration_accept

    subroutine parameter_declaration_accept(this, visitor)
        class(parameter_declaration_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine parameter_declaration_accept

    subroutine do_loop_accept(this, visitor)
        class(do_loop_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine do_loop_accept

    subroutine do_while_accept(this, visitor)
        class(do_while_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine do_while_accept

    subroutine if_accept(this, visitor)
        class(if_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine if_accept

    subroutine select_case_accept(this, visitor)
        class(select_case_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine select_case_accept

    subroutine derived_type_accept(this, visitor)
        class(derived_type_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine derived_type_accept

    subroutine interface_block_accept(this, visitor)
        class(interface_block_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine interface_block_accept

    subroutine module_accept(this, visitor)
        class(module_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine module_accept

    ! Deep copy an AST entry
    function ast_entry_deep_copy(this) result(copy)
        class(ast_entry_t), intent(in) :: this
        type(ast_entry_t) :: copy
        integer :: i

        ! Copy scalar fields
        copy%parent_index = this%parent_index
        copy%depth = this%depth
        copy%child_count = this%child_count

        ! Deep copy allocatable components
        if (allocated(this%node_type)) then
            copy%node_type = this%node_type
        end if

        if (allocated(this%child_indices)) then
            allocate (copy%child_indices(size(this%child_indices)))
            copy%child_indices = this%child_indices
        end if

        ! Note: We cannot deep copy the polymorphic node component
        ! as it would require knowledge of all possible derived types.
        ! This should be handled at a higher level if needed.
        if (allocated(this%node)) then
            ! For now, we'll skip deep copying the node itself
            ! This is a limitation that needs to be addressed based on usage
        end if
    end function ast_entry_deep_copy

    ! Assignment operator for ast_entry_t (deep copy)
    subroutine ast_entry_assign(lhs, rhs)
        class(ast_entry_t), intent(out) :: lhs
        type(ast_entry_t), intent(in) :: rhs
        integer :: i

        ! Copy scalar fields
        lhs%parent_index = rhs%parent_index
        lhs%depth = rhs%depth
        lhs%child_count = rhs%child_count

        ! Deep copy allocatable components
        if (allocated(rhs%node_type)) then
            lhs%node_type = rhs%node_type
        end if

        if (allocated(rhs%child_indices)) then
            allocate (lhs%child_indices(size(rhs%child_indices)))
            lhs%child_indices = rhs%child_indices
        end if

        ! Note: See comment in deep_copy about node component
        if (allocated(rhs%node)) then
            ! For now, we'll skip deep copying the node itself
            ! This is a limitation that needs to be addressed based on usage
        end if
    end subroutine ast_entry_assign

    ! Deep copy an AST arena
    function ast_arena_deep_copy(this) result(copy)
        class(ast_arena_t), intent(in) :: this
        type(ast_arena_t) :: copy
        integer :: i

        ! Copy scalar fields
        copy%size = this%size
        copy%capacity = this%capacity
        copy%current_index = this%current_index
        copy%max_depth = this%max_depth
        copy%chunk_size = this%chunk_size
        copy%initial_capacity = this%initial_capacity

        ! Deep copy entries array
        if (allocated(this%entries)) then
            allocate (copy%entries(size(this%entries)))
            do i = 1, size(this%entries)
                copy%entries(i) = this%entries(i)  ! Uses ast_entry assignment
            end do
        end if
    end function ast_arena_deep_copy

    ! Assignment operator for ast_arena_t (deep copy)
    subroutine ast_arena_assign(lhs, rhs)
        class(ast_arena_t), intent(out) :: lhs
        type(ast_arena_t), intent(in) :: rhs
        integer :: i

        ! Copy scalar fields
        lhs%size = rhs%size
        lhs%capacity = rhs%capacity
        lhs%current_index = rhs%current_index
        lhs%max_depth = rhs%max_depth
        lhs%chunk_size = rhs%chunk_size
        lhs%initial_capacity = rhs%initial_capacity

        ! Deep copy entries array
        if (allocated(rhs%entries)) then
            allocate (lhs%entries(size(rhs%entries)))
            do i = 1, size(rhs%entries)
                lhs%entries(i) = rhs%entries(i)  ! Uses ast_entry assignment
            end do
        end if
    end subroutine ast_arena_assign

end module ast_core
