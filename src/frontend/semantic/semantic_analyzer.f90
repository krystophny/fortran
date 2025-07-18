module semantic_analyzer
    ! Hindley-Milner type inference (Algorithm W) - dialect-agnostic
    use type_system_hm, only: type_env_t, type_var_t, mono_type_t, poly_type_t, &
                              substitution_t, create_mono_type, create_type_var, &
                              create_poly_type, create_fun_type, free_type_vars, &
                              compose_substitutions, occurs_check, &
                              TVAR, TINT, TREAL, TCHAR, TFUN, TARRAY
    use scope_manager
    use type_checker
    use ast_core
    implicit none
    private

    public :: semantic_context_t, create_semantic_context
    public :: analyze_program

    ! Semantic analysis context
    type :: semantic_context_t
        type(type_env_t) :: env  ! Legacy flat environment (to be removed)
        type(scope_stack_t) :: scopes  ! Hierarchical scope management
        integer :: next_var_id = 0
        type(substitution_t) :: subst
    contains
        procedure :: infer => infer_type
        procedure :: infer_stmt => infer_statement_type
        procedure :: unify => unify_types
        procedure :: instantiate => instantiate_type_scheme
        procedure :: generalize => generalize_type
        procedure :: fresh_type_var => generate_fresh_type_var
        procedure :: apply_subst_to_type => apply_current_substitution
        procedure :: get_builtin_function_type
        procedure :: compose_with_subst
        procedure :: deep_copy => semantic_context_deep_copy
        procedure :: assign => semantic_context_assign
        generic :: assignment(=) => assign
    end type semantic_context_t

contains

    ! Create a new semantic context with builtin functions
    function create_semantic_context() result(ctx)
        type(semantic_context_t) :: ctx
        type(poly_type_t) :: builtin_scheme
        type(mono_type_t) :: real_to_real, real_type

        ! Initialize substitution
        ctx%subst%count = 0

        ! Initialize hierarchical scope stack
        ctx%scopes = create_scope_stack()

        ! Initialize legacy flat environment
        ctx%env%count = 0
        ctx%env%capacity = 10
        allocate (character(len=256) :: ctx%env%names(ctx%env%capacity))
        allocate (ctx%env%schemes(ctx%env%capacity))

        ! Initialize environment with builtin functions
        ctx%next_var_id = 1

        ! Create real -> real type for math functions
        real_type = create_mono_type(TREAL)
        real_to_real = create_fun_type(real_type, real_type)

        ! Create polymorphic type scheme (no type variables to generalize)
        builtin_scheme = create_poly_type(forall_vars=[type_var_t::], mono=real_to_real)

        ! Add common math functions to global scope
        call ctx%scopes%define("sin", builtin_scheme)
        call ctx%scopes%define("cos", builtin_scheme)
        call ctx%scopes%define("tan", builtin_scheme)
        call ctx%scopes%define("sqrt", builtin_scheme)
        call ctx%scopes%define("exp", builtin_scheme)
        call ctx%scopes%define("log", builtin_scheme)
        call ctx%scopes%define("abs", builtin_scheme)

        ! Also add to legacy flat environment for compatibility
        call ctx%env%extend("sin", builtin_scheme)
        call ctx%env%extend("cos", builtin_scheme)
        call ctx%env%extend("tan", builtin_scheme)
        call ctx%env%extend("sqrt", builtin_scheme)
        call ctx%env%extend("exp", builtin_scheme)
        call ctx%env%extend("log", builtin_scheme)
        call ctx%env%extend("abs", builtin_scheme)

    end function create_semantic_context

    ! Main entry point: analyze entire program
    subroutine analyze_program(ctx, arena, root_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index

        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        select type (ast => arena%entries(root_index)%node)
        type is (program_node)
            call analyze_program_node_arena(ctx, arena, ast, root_index)
        class default
            ! Single statement/expression
            call infer_and_store_type(ctx, arena, root_index)
        end select
    end subroutine analyze_program

    ! Analyze a program node with arena-based AST
    subroutine analyze_program_node_arena(ctx, arena, prog, prog_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index
        integer :: i

        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
             if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                    call infer_and_store_type(ctx, arena, prog%body_indices(i))
                end if
            end do
        end if
    end subroutine analyze_program_node_arena

    ! Analyze a program node (legacy interface)
    subroutine analyze_program_node(ctx, prog)
        type(semantic_context_t), intent(inout) :: ctx
        type(program_node), intent(inout) :: prog
        integer :: i

        if (allocated(prog%body_indices)) then
            ! This is an arena-based program node but called without arena
            ! For now, skip analysis - this should be updated to use analyze_program_arena
            return
        end if
    end subroutine analyze_program_node

    ! Infer type and store in AST node
    subroutine infer_and_store_type(ctx, arena, node_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(mono_type_t) :: inferred

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        inferred = ctx%infer_stmt(arena, node_index)

        ! Store the inferred type in the AST node (assignment now does deep copy automatically)
        if (.not. allocated(arena%entries(node_index)%node%inferred_type)) then
            allocate (arena%entries(node_index)%node%inferred_type)
        end if
        arena%entries(node_index)%node%inferred_type = inferred
    end subroutine infer_and_store_type

    ! Infer type of a statement
    function infer_statement_type(this, arena, stmt_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        type(mono_type_t) :: typ

        if (stmt_index <= 0 .or. stmt_index > arena%size) then
            typ = create_mono_type(TINT)  ! Default fallback
            return
        end if
        if (.not. allocated(arena%entries(stmt_index)%node)) then
            typ = create_mono_type(TINT)  ! Default fallback
            return
        end if

        select type (stmt => arena%entries(stmt_index)%node)
        type is (assignment_node)
            typ = infer_assignment(this, arena, stmt, stmt_index)
            ! Store inference metadata in the assignment_node for error detection
            stmt%type_was_inferred = .true.
            stmt%inferred_type_name = typ%to_string()
        type is (print_statement_node)
            typ = create_mono_type(TINT)  ! print returns unit/void, use int
        type is (module_node)
            typ = analyze_module(this, arena, stmt, stmt_index)
        type is (function_def_node)
            typ = analyze_function_def(this, arena, stmt, stmt_index)
        type is (subroutine_def_node)
            typ = analyze_subroutine_def(this, arena, stmt, stmt_index)
        type is (if_node)
            typ = analyze_if_node(this, arena, stmt, stmt_index)
        type is (do_loop_node)
            typ = analyze_do_loop(this, arena, stmt, stmt_index)
        type is (do_while_node)
            typ = analyze_do_while(this, arena, stmt, stmt_index)
        class default
            ! For expressions, use general inference
            typ = this%infer(arena, stmt_index)
        end select
    end function infer_statement_type

    ! Infer type of assignment and update environment
    function infer_assignment(ctx, arena, assign, assign_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(inout) :: assign
        integer, intent(in) :: assign_index
        type(mono_type_t) :: typ, target_type
        type(poly_type_t) :: scheme
        type(poly_type_t), allocatable :: existing_scheme
        character(len=:), allocatable :: var_name

        ! Infer type of RHS
        typ = ctx%infer(arena, assign%value_index)

        ! Get variable name from target
        if (assign%target_index > 0 .and. assign%target_index <= arena%size) then
            select type (target => arena%entries(assign%target_index)%node)
            type is (identifier_node)
                var_name = target%name

                ! Check if variable already exists
                call ctx%scopes%lookup(var_name, existing_scheme)

                if (allocated(existing_scheme)) then
                    ! Variable exists - check assignment compatibility
                    target_type = ctx%instantiate(existing_scheme)

                    if (.not. is_assignable(typ, target_type)) then
                        ! Type error - for now, just continue with inference
                        ! In a full implementation, we would report an error
                        ! error stop type_error(target_type, typ, "assignment to " // var_name)
                    end if

                    ! Use the existing type for consistency
                    typ = target_type
                end if

                ! Store type in the identifier node
                if (.not. allocated(target%inferred_type)) then
                    allocate (target%inferred_type)
                end if
                target%inferred_type = typ

                ! If new variable, generalize and add to environment
                if (.not. allocated(existing_scheme)) then
                    scheme = ctx%generalize(typ)
                    call ctx%scopes%define(var_name, scheme)

                    ! Also add to legacy flat environment for compatibility
                    call ctx%env%extend(var_name, scheme)
                end if
            class default
                error stop "Assignment target must be identifier"
            end select
        end if
    end function infer_assignment

    ! Main type inference function (Algorithm W)
    recursive function infer_type(this, arena, expr_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: expr_index
        type(mono_type_t) :: typ

        if (expr_index <= 0 .or. expr_index > arena%size) then
            typ = create_mono_type(TREAL)
            return
        end if
        if (.not. allocated(arena%entries(expr_index)%node)) then
            typ = create_mono_type(TREAL)
            return
        end if

        select type (expr => arena%entries(expr_index)%node)
        type is (literal_node)
            typ = infer_literal(this, expr)

        type is (identifier_node)
            typ = infer_identifier(this, expr)

        type is (binary_op_node)
            typ = infer_binary_op(this, arena, expr, expr_index)

        type is (call_or_subscript_node)
            typ = infer_function_call(this, arena, expr)

        type is (subroutine_call_node)
            ! Subroutine calls don't return a value - shouldn't appear in expressions
            ! Return a type variable that will fail type checking
            typ = create_mono_type(TVAR, var=create_type_var(0, "error"))

        type is (assignment_node)
            typ = infer_assignment(this, arena, expr, expr_index)

        class default
            ! Return real type as default for unsupported expressions
            typ = create_mono_type(TREAL)
        end select

        ! Apply current substitution
        typ = this%apply_subst_to_type(typ)

        ! Store the inferred type in the AST node
        if (.not. allocated(arena%entries(expr_index)%node%inferred_type)) then
            allocate (arena%entries(expr_index)%node%inferred_type)
        end if
        arena%entries(expr_index)%node%inferred_type = typ
    end function infer_type

    ! Infer type of literal
    function infer_literal(ctx, lit) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(literal_node), intent(in) :: lit
        type(mono_type_t) :: typ

        select case (lit%literal_kind)
        case (LITERAL_INTEGER)
            typ = create_mono_type(TINT)
        case (LITERAL_REAL)
            typ = create_mono_type(TREAL)
        case (LITERAL_STRING)
            ! Calculate string length (subtract 2 for quotes)
            typ = create_mono_type(TCHAR, char_size=len_trim(lit%value) - 2)
        case (LITERAL_LOGICAL)
            typ = create_mono_type(TINT)  ! Boolean as integer
        case default
            error stop "Unknown literal kind"
        end select
    end function infer_literal

    ! Infer type of identifier
    function infer_identifier(ctx, ident) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(identifier_node), intent(in) :: ident
        type(mono_type_t) :: typ
        type(poly_type_t), allocatable :: scheme

        ! Safety check: ensure identifier name is allocated and not empty
        if (.not. allocated(ident%name) .or. len_trim(ident%name) == 0) then
            typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
            return
        end if

        ! Look up identifier in hierarchical scopes
        call ctx%scopes%lookup(ident%name, scheme)

        if (allocated(scheme)) then
            ! Found in environment - instantiate the type scheme
            typ = ctx%instantiate(scheme)
        else
            ! Not found - create fresh type variable
            typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
        end if
    end function infer_identifier

    ! Infer type of binary operation
    function infer_binary_op(ctx, arena, binop, binop_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(binary_op_node), intent(inout) :: binop
        integer, intent(in) :: binop_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: left_typ, right_typ, result_typ
        type(substitution_t) :: s1, s2, s3
        integer :: compat_level

        ! Infer left operand type
        left_typ = ctx%infer(arena, binop%left_index)

        ! Infer right operand type
        right_typ = ctx%infer(arena, binop%right_index)

        ! Determine result type based on operator
        select case (trim(binop%operator))
        case ("+", "-", "*", "/", "**")
            ! Numeric operations: check compatibility
            if (is_compatible(left_typ, right_typ, compat_level)) then
                if (is_numeric_type(left_typ) .or. is_numeric_type(right_typ)) then
                    ! Get common type for numeric operations
                    result_typ = get_common_type(left_typ, right_typ)
                else
                    ! For type variables, unify as before
                    result_typ = create_mono_type(TVAR, var=ctx%fresh_type_var())

                    ! Unify left with result
                    s1 = ctx%unify(left_typ, result_typ)
                    call ctx%compose_with_subst(s1)

                    ! Apply s1 to right_typ before unifying
                    call s1%apply(right_typ, right_typ)
                    call s1%apply(result_typ, result_typ)

                    ! Unify right with result
                    s2 = ctx%unify(right_typ, result_typ)
                    call ctx%compose_with_subst(s2)

                    ! Final result type
                    call s2%apply(result_typ, result_typ)
                end if
            else
                ! Type error - for now, return real as default
                result_typ = create_mono_type(TREAL)
            end if

        case ("<", ">", "<=", ">=", "==", "/=")
            ! Comparison operations: operands must be compatible
            if (is_compatible(left_typ, right_typ, compat_level)) then
                result_typ = create_mono_type(TINT)  ! Boolean as integer
            else
                ! Type error - still return boolean
                result_typ = create_mono_type(TINT)
            end if

        case (".and.", ".or.")
            ! Logical operations: all integer (boolean)
            s1 = ctx%unify(left_typ, create_mono_type(TINT))
            call ctx%compose_with_subst(s1)
            s2 = ctx%unify(right_typ, create_mono_type(TINT))
            call ctx%compose_with_subst(s2)
            result_typ = create_mono_type(TINT)

        case default
            error stop "Unknown binary operator: "//trim(binop%operator)
        end select

        typ = ctx%apply_subst_to_type(result_typ)
    end function infer_binary_op

    ! Infer type of function call
    function infer_function_call(ctx, arena, call_node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(inout) :: call_node
        type(mono_type_t) :: typ
        type(mono_type_t) :: fun_typ, arg_typ, result_typ
        type(mono_type_t), allocatable :: arg_types(:)
        type(substitution_t) :: s
        integer :: i

        ! Get function type
        fun_typ = ctx%get_builtin_function_type(call_node%name)

        if (fun_typ%kind == 0) then
            ! Unknown function - look up in environment
            block
                type(identifier_node) :: fun_ident
                fun_ident = create_identifier(call_node%name, &
                                              call_node%line, call_node%column)
                fun_typ = infer_identifier(ctx, fun_ident)
            end block
        end if

        ! Process arguments
        if (allocated(call_node%arg_indices)) then
            allocate (arg_types(size(call_node%arg_indices)))

            ! Infer all argument types
            do i = 1, size(call_node%arg_indices)
                arg_types(i) = infer_type(ctx, arena, call_node%arg_indices(i))
            end do

            ! Unify with function type
            result_typ = fun_typ
            do i = 1, size(arg_types)
                ! Create expected function type: arg -> result
                block
                    type(type_var_t) :: tv
                    type(mono_type_t) :: expected_fun_type, new_result_typ

                    tv = ctx%fresh_type_var()
                    new_result_typ = create_mono_type(TVAR, var=tv)
                    expected_fun_type = create_fun_type(arg_types(i), new_result_typ)

                    ! Unify current function type with expected
                    s = ctx%unify(result_typ, expected_fun_type)
                    call ctx%compose_with_subst(s)

                    ! Update result type
                    call s%apply(new_result_typ, result_typ)
                end block
            end do

            typ = result_typ
        else
            ! No arguments - function type is the result
            if (fun_typ%kind == TFUN .and. allocated(fun_typ%args)) then
                typ = fun_typ%args(size(fun_typ%args))  ! Last element is return type
            else
                typ = fun_typ
            end if
        end if

        typ = ctx%apply_subst_to_type(typ)
    end function infer_function_call

    ! Type unification
    recursive function unify_types(this, t1, t2) result(subst)
        class(semantic_context_t), intent(inout) :: this
        type(mono_type_t), intent(in) :: t1, t2
        type(substitution_t) :: subst
        type(mono_type_t) :: t1_subst, t2_subst

        ! Apply current substitution first
        t1_subst = this%apply_subst_to_type(t1)
        t2_subst = this%apply_subst_to_type(t2)

        ! Initialize empty substitution
        subst%count = 0

        ! Handle type variables
        if (t1_subst%kind == TVAR) then
            if (t2_subst%kind == TVAR .and. t1_subst%var%id == t2_subst%var%id) then
                ! Same variable - empty substitution
                return
            else if (occurs_check(t1_subst%var, t2_subst)) then
                error stop "Occurs check failed - infinite type"
            else
                call subst%add(t1_subst%var, t2_subst)
            end if
            return
        else if (t2_subst%kind == TVAR) then
            if (occurs_check(t2_subst%var, t1_subst)) then
                error stop "Occurs check failed - infinite type"
            else
                call subst%add(t2_subst%var, t1_subst)
            end if
            return
        end if

        ! Both are concrete types
        if (t1_subst%kind /= t2_subst%kind) then
            error stop "Type mismatch: cannot unify "// &
                t1_subst%to_string()//" with "//t2_subst%to_string()
        end if

        select case (t1_subst%kind)
        case (TINT, TREAL)
            ! Base types unify if equal (already checked kind)

        case (TCHAR)
            if (t1_subst%size /= t2_subst%size) then
                error stop "Cannot unify character types of different lengths"
            end if

        case (TFUN)
            if (.not. allocated(t1_subst%args) .or. .not. allocated(t2_subst%args)) then
                error stop "Invalid function types"
            end if
            if (size(t1_subst%args) /= size(t2_subst%args)) then
                error stop "Function arity mismatch"
            end if

            ! Unify arguments pairwise
            block
                integer :: i
                type(substitution_t) :: s

                do i = 1, size(t1_subst%args)
                    s = this%unify(this%apply_subst_to_type(t1_subst%args(i)), &
                                   this%apply_subst_to_type(t2_subst%args(i)))
                    subst = compose_substitutions(s, subst)
                    call this%compose_with_subst(s)
                end do
            end block

        case (TARRAY)
            if (.not. allocated(t1_subst%args) .or. .not. allocated(t2_subst%args)) then
                error stop "Invalid array types"
            end if

            ! Unify element types
            subst = this%unify(t1_subst%args(1), t2_subst%args(1))

            ! Check sizes if known
            if (t1_subst%size > 0 .and. t2_subst%size > 0) then
                if (t1_subst%size /= t2_subst%size) then
                    error stop "Cannot unify arrays of different sizes"
                end if
            end if

        case default
            error stop "Unknown type kind in unification"
        end select
    end function unify_types

    ! Instantiate a type scheme
    function instantiate_type_scheme(this, scheme) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(poly_type_t), intent(in) :: scheme
        type(mono_type_t) :: typ
        type(substitution_t) :: subst
        integer :: i

        ! Create fresh type variables for all quantified variables
        subst%count = 0
        if (allocated(scheme%forall)) then
            do i = 1, size(scheme%forall)
                call subst%add(scheme%forall(i), &
                               create_mono_type(TVAR, var=this%fresh_type_var()))
            end do
        end if

        ! Apply substitution to get instance
        call subst%apply(scheme%mono, typ)
    end function instantiate_type_scheme

    ! Generalize a type to a type scheme
    function generalize_type(this, typ) result(scheme)
        class(semantic_context_t), intent(inout) :: this
        type(mono_type_t), intent(in) :: typ
        type(poly_type_t) :: scheme
        type(type_var_t), allocatable :: free_vars(:), env_vars(:), gen_vars(:)
        integer :: i, j, count
        logical :: in_env

        ! Get free variables in type
        call free_type_vars(typ, free_vars)

        if (size(free_vars) == 0) then
            ! No free variables - monomorphic type
            scheme = create_poly_type(forall_vars=[type_var_t::], mono=typ)
            return
        end if

        ! Get free variables in environment
        call get_env_free_vars(this%env, env_vars)

        ! Find variables to generalize (in type but not in env)
        allocate (gen_vars(size(free_vars)))
        count = 0

        do i = 1, size(free_vars)
            in_env = .false.
            do j = 1, size(env_vars)
                if (free_vars(i)%id == env_vars(j)%id) then
                    in_env = .true.
                    exit
                end if
            end do

            if (.not. in_env) then
                count = count + 1
                gen_vars(count) = free_vars(i)
            end if
        end do

        ! Create type scheme
        if (count > 0) then
            scheme = create_poly_type(forall_vars=gen_vars(1:count), mono=typ)
        else
            scheme = create_poly_type(forall_vars=[type_var_t::], mono=typ)
        end if
    end function generalize_type

    ! Generate fresh type variable
    function generate_fresh_type_var(this) result(tv)
        class(semantic_context_t), intent(inout) :: this
        type(type_var_t) :: tv

        this%next_var_id = this%next_var_id + 1
        tv = create_type_var(this%next_var_id)
    end function generate_fresh_type_var

    ! Apply current substitution to a type
    function apply_current_substitution(this, typ) result(result_typ)
        class(semantic_context_t), intent(in) :: this
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: result_typ

        call this%subst%apply(typ, result_typ)
    end function apply_current_substitution

    ! Compose a substitution with the current one
    subroutine compose_with_subst(this, s)
        class(semantic_context_t), intent(inout) :: this
        type(substitution_t), intent(in) :: s

        this%subst = compose_substitutions(s, this%subst)
    end subroutine compose_with_subst

    ! Get builtin function type
    function get_builtin_function_type(this, name) result(typ)
        class(semantic_context_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(mono_type_t) :: typ
        type(mono_type_t) :: int_type, real_type

        ! Safety check: ensure name is not empty
        if (len_trim(name) == 0) then
            typ%kind = 0
            return
        end if

        ! Create basic types
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)

        ! Return appropriate function types for intrinsics
        select case (trim(name))
            ! Real -> Real functions
        case ("sqrt", "sin", "cos", "tan", "exp", "log", "asin", "acos", "atan", &
              "sinh", "cosh", "tanh", "asinh", "acosh", "atanh")
            typ = create_fun_type(real_type, real_type)

            ! Abs can take integer or real (polymorphic - for now just real)
        case ("abs")
            typ = create_fun_type(real_type, real_type)

            ! Integer functions
        case ("int", "floor", "ceiling", "nint")
            typ = create_fun_type(real_type, int_type)

            ! Real conversion
        case ("real", "float")
            typ = create_fun_type(int_type, real_type)

            ! Min/max (for now, just real -> real)
        case ("min", "max")
            typ = create_fun_type(real_type, real_type)

            ! Mod function
        case ("mod", "modulo")
            ! Two arguments - for now simplified as real -> real
            typ = create_fun_type(real_type, real_type)

            ! Precision inquiry function (real -> integer)
        case ("precision")
            typ = create_fun_type(real_type, int_type)

        case default
            ! Return empty type to indicate not found
            typ%kind = 0
        end select
    end function get_builtin_function_type

    ! Get free type variables in environment
    subroutine get_env_free_vars(env, vars)
        type(type_env_t), intent(in) :: env
        type(type_var_t), allocatable, intent(out) :: vars(:)
        type(type_var_t), allocatable :: temp_vars(:), scheme_vars(:)
        integer :: i, j, k, count
        logical :: found

        allocate (temp_vars(1000))  ! Temporary storage
        count = 0

        ! Collect all free variables from all schemes
        do i = 1, env%count
            call get_scheme_free_vars(env%schemes(i), scheme_vars)

            do j = 1, size(scheme_vars)
                ! Check if already collected
                found = .false.
                do k = 1, count
                    if (temp_vars(k)%id == scheme_vars(j)%id) then
                        found = .true.
                        exit
                    end if
                end do

                if (.not. found) then
                    count = count + 1
                    temp_vars(count) = scheme_vars(j)
                end if
            end do
        end do

        ! Return exact size array
        if (count > 0) then
            allocate (vars(count))
            vars = temp_vars(1:count)
        else
            allocate (vars(0))
        end if
    end subroutine get_env_free_vars

    ! Get free variables in a type scheme
    subroutine get_scheme_free_vars(scheme, vars)
        type(poly_type_t), intent(in) :: scheme
        type(type_var_t), allocatable, intent(out) :: vars(:)
        type(type_var_t), allocatable :: mono_vars(:)
        integer :: i, j, count
        logical :: quantified

        ! Get free variables in monotype
        call free_type_vars(scheme%mono, mono_vars)

        if (.not. allocated(scheme%forall) .or. size(scheme%forall) == 0) then
            vars = mono_vars
            return
        end if

        ! Filter out quantified variables (safe allocation)
        allocate (vars(size(mono_vars)))
        count = 0

        do i = 1, size(mono_vars)
            quantified = .false.
            do j = 1, size(scheme%forall)
                if (mono_vars(i)%id == scheme%forall(j)%id) then
                    quantified = .true.
                    exit
                end if
            end do

            if (.not. quantified) then
                count = count + 1
                vars(count) = mono_vars(i)
            end if
        end do

        ! Return exact size (safe array resizing)
        if (count > 0) then
            block
                type(type_var_t), allocatable :: temp(:)
                allocate (temp(count))
                temp = vars(1:count)
                deallocate (vars)
                allocate (vars(count))
                vars = temp
            end block
        else
            ! Deallocate and allocate empty array
            deallocate (vars)
            allocate (vars(0))
        end if
    end subroutine get_scheme_free_vars

    ! Analyze module node
    function analyze_module(ctx, arena, mod_node, mod_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(module_node), intent(inout) :: mod_node
        integer, intent(in) :: mod_index
        type(mono_type_t) :: typ
        integer :: i

        ! Enter module scope
        call ctx%scopes%enter_module(mod_node%name)

        ! Analyze module declarations
        if (allocated(mod_node%declaration_indices)) then
            do i = 1, size(mod_node%declaration_indices)
                if (mod_node%declaration_indices(i) > 0) then
                    block
                        type(mono_type_t) :: decl_type
                        decl_type = ctx%infer(arena, mod_node%declaration_indices(i))
                    end block
                end if
            end do
        end if

        ! Analyze module procedures
        if (allocated(mod_node%procedure_indices)) then
            do i = 1, size(mod_node%procedure_indices)
                if (mod_node%procedure_indices(i) > 0) then
                    block
                        type(mono_type_t) :: proc_type
                        proc_type = ctx%infer(arena, mod_node%procedure_indices(i))
                    end block
                end if
            end do
        end if

        ! Leave module scope
        call ctx%scopes%leave_scope()

        ! Modules don't have a type value
        typ = create_mono_type(TINT)  ! Unit type
    end function analyze_module

    ! Analyze function definition
    function analyze_function_def(ctx, arena, func_def, func_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        type(mono_type_t) :: typ, param_type, return_type
        type(mono_type_t), allocatable :: param_types(:)
        type(poly_type_t) :: func_scheme
        integer :: i

        ! Enter function scope
        call ctx%scopes%enter_function(func_def%name)

        ! Process parameters and add to local scope
        if (allocated(func_def%param_indices)) then
            allocate (param_types(size(func_def%param_indices)))
            do i = 1, size(func_def%param_indices)
                ! For now, assign fresh type variables to parameters
                param_types(i) = create_mono_type(TVAR, var=ctx%fresh_type_var())

                ! Add parameter to local scope - get from arena
                if (allocated(arena%entries(func_def%param_indices(i))%node)) then
                    select type (param => arena%entries(func_def%param_indices(i))%node)
                    type is (identifier_node)
                        call ctx%scopes%define(param%name, &
                      create_poly_type(forall_vars=[type_var_t::], mono=param_types(i)))
                    end select
                end if
            end do
        else
            allocate (param_types(0))
        end if

        ! Analyze function body statements
        if (allocated(func_def%body_indices)) then
            do i = 1, size(func_def%body_indices)
                ! Analyze body statement using arena indexing
                call infer_and_store_type(ctx, arena, func_def%body_indices(i))
            end do
        end if

        ! Infer return type (for now, use a fresh type variable)
        return_type = create_mono_type(TVAR, var=ctx%fresh_type_var())

        ! Build function type
        if (size(param_types) == 0) then
            typ = return_type
        else if (size(param_types) == 1) then
            typ = create_fun_type(param_types(1), return_type)
        else
            ! Multi-argument function - curry
            typ = param_types(size(param_types))
            do i = size(param_types) - 1, 1, -1
                typ = create_fun_type(param_types(i), typ)
            end do
            typ = create_fun_type(param_types(1), typ)
        end if

        ! Leave function scope
        call ctx%scopes%leave_scope()

        ! Add function to parent scope
        func_scheme = ctx%generalize(typ)
        call ctx%scopes%define(func_def%name, func_scheme)

    end function analyze_function_def

    ! Analyze subroutine definition
    function analyze_subroutine_def(ctx, arena, sub_def, sub_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(inout) :: sub_def
        integer, intent(in) :: sub_index
        type(mono_type_t) :: typ
        type(poly_type_t) :: sub_scheme
        integer :: i

        ! Enter subroutine scope
        call ctx%scopes%enter_subroutine(sub_def%name)

        ! Process parameters and add to local scope
        if (allocated(sub_def%param_indices)) then
            do i = 1, size(sub_def%param_indices)
                ! For now, assign fresh type variables to parameters
                if (allocated(arena%entries(sub_def%param_indices(i))%node)) then
                    select type (param => arena%entries(sub_def%param_indices(i))%node)
                    type is (identifier_node)
                        call ctx%scopes%define(param%name, &
                                          create_poly_type(forall_vars=[type_var_t::], &
                                 mono=create_mono_type(TVAR, var=ctx%fresh_type_var())))
                    end select
                end if
            end do
        end if

        ! Analyze subroutine body statements
        if (allocated(sub_def%body_indices)) then
            do i = 1, size(sub_def%body_indices)
                ! Analyze body statement using arena indexing
                call infer_and_store_type(ctx, arena, sub_def%body_indices(i))
            end do
        end if

        ! Leave subroutine scope
        call ctx%scopes%leave_scope()

        ! Subroutines have unit type
        typ = create_mono_type(TINT)  ! Unit type

        ! Add subroutine to parent scope
        sub_scheme = create_poly_type(forall_vars=[type_var_t::], mono=typ)
        call ctx%scopes%define(sub_def%name, sub_scheme)

    end function analyze_subroutine_def

    ! Analyze if node with block scopes
    function analyze_if_node(ctx, arena, if_stmt, if_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(if_node), intent(inout) :: if_stmt
        integer, intent(in) :: if_index
        type(mono_type_t) :: typ
        integer :: i, j

        ! Analyze condition
        if (if_stmt%condition_index > 0) then
            block
                type(mono_type_t) :: cond_type
                cond_type = ctx%infer(arena, if_stmt%condition_index)
                ! Condition should be logical type
            end block
        end if

        ! Enter then block scope
        call ctx%scopes%enter_block()

        ! Analyze then body statements
        if (allocated(if_stmt%then_body_indices)) then
            do i = 1, size(if_stmt%then_body_indices)
                if (if_stmt%then_body_indices(i) > 0) then
                    block
                        type(mono_type_t) :: stmt_type
                        stmt_type = ctx%infer(arena, if_stmt%then_body_indices(i))
                    end block
                end if
            end do
        end if

        ! Leave then block scope
        call ctx%scopes%leave_scope()

        ! Analyze elseif blocks
        if (allocated(if_stmt%elseif_blocks)) then
            do i = 1, size(if_stmt%elseif_blocks)
                call ctx%scopes%enter_block()

                ! Analyze elseif condition
                if (if_stmt%elseif_blocks(i)%condition_index > 0) then
                    block
                        type(mono_type_t) :: cond_type
                  cond_type = ctx%infer(arena, if_stmt%elseif_blocks(i)%condition_index)
                    end block
                end if

                ! Analyze elseif body
                if (allocated(if_stmt%elseif_blocks(i)%body_indices)) then
                    do j = 1, size(if_stmt%elseif_blocks(i)%body_indices)
                        if (if_stmt%elseif_blocks(i)%body_indices(j) > 0) then
                            block
                                type(mono_type_t) :: stmt_type
                  stmt_type = ctx%infer(arena, if_stmt%elseif_blocks(i)%body_indices(j))
                            end block
                        end if
                    end do
                end if

                call ctx%scopes%leave_scope()
            end do
        end if

        ! Analyze else block
        if (allocated(if_stmt%else_body_indices)) then
            call ctx%scopes%enter_block()
            do i = 1, size(if_stmt%else_body_indices)
                if (if_stmt%else_body_indices(i) > 0) then
                    block
                        type(mono_type_t) :: stmt_type
                        stmt_type = ctx%infer(arena, if_stmt%else_body_indices(i))
                    end block
                end if
            end do
            call ctx%scopes%leave_scope()
        end if

        ! If statements have unit type
        typ = create_mono_type(TINT)  ! Unit type
    end function analyze_if_node

    ! Analyze do loop with block scope
    function analyze_do_loop(ctx, arena, do_stmt, do_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(do_loop_node), intent(inout) :: do_stmt
        integer, intent(in) :: do_index
        type(mono_type_t) :: typ, loop_var_type
        type(poly_type_t) :: loop_var_scheme
        integer :: i

        ! Enter loop block scope
        call ctx%scopes%enter_block()

        ! Add loop variable to scope
        loop_var_type = create_mono_type(TINT)  ! Loop variables are integers
      loop_var_scheme = create_poly_type(forall_vars=[type_var_t::], mono=loop_var_type)
        call ctx%scopes%define(do_stmt%var_name, loop_var_scheme)

        ! Analyze loop bounds
        if (do_stmt%start_expr_index > 0) then
            block
                type(mono_type_t) :: start_type
                start_type = ctx%infer(arena, do_stmt%start_expr_index)
            end block
        end if
        if (do_stmt%end_expr_index > 0) then
            block
                type(mono_type_t) :: end_type
                end_type = ctx%infer(arena, do_stmt%end_expr_index)
            end block
        end if
        if (do_stmt%step_expr_index > 0) then
            block
                type(mono_type_t) :: step_type
                step_type = ctx%infer(arena, do_stmt%step_expr_index)
            end block
        end if

        ! Analyze loop body
        if (allocated(do_stmt%body_indices)) then
            do i = 1, size(do_stmt%body_indices)
                if (do_stmt%body_indices(i) > 0) then
                    block
                        type(mono_type_t) :: stmt_type
                        stmt_type = ctx%infer(arena, do_stmt%body_indices(i))
                    end block
                end if
            end do
        end if

        ! Leave loop block scope
        call ctx%scopes%leave_scope()

        ! Do loops have unit type
        typ = create_mono_type(TINT)  ! Unit type
    end function analyze_do_loop

    ! Analyze do while loop with block scope
    function analyze_do_while(ctx, arena, do_while_stmt, do_while_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(do_while_node), intent(inout) :: do_while_stmt
        integer, intent(in) :: do_while_index
        type(mono_type_t) :: typ
        integer :: i

        ! Enter loop block scope
        call ctx%scopes%enter_block()

        ! Analyze condition
        if (do_while_stmt%condition_index > 0) then
            block
                type(mono_type_t) :: cond_type
                cond_type = ctx%infer(arena, do_while_stmt%condition_index)
            end block
        end if

        ! Analyze loop body
        if (allocated(do_while_stmt%body_indices)) then
            do i = 1, size(do_while_stmt%body_indices)
                if (do_while_stmt%body_indices(i) > 0) then
                    block
                        type(mono_type_t) :: stmt_type
                        stmt_type = ctx%infer(arena, do_while_stmt%body_indices(i))
                    end block
                end if
            end do
        end if

        ! Leave loop block scope
        call ctx%scopes%leave_scope()

        ! Do while loops have unit type
        typ = create_mono_type(TINT)  ! Unit type
    end function analyze_do_while

    ! Deep copy procedures for semantic_context_t
    function semantic_context_deep_copy(this) result(copy)
        class(semantic_context_t), intent(in) :: this
        type(semantic_context_t) :: copy

        copy%env = this%env              ! Uses type_env_t assignment (deep copy)
        copy%scopes = this%scopes        ! Uses scope_stack_t assignment (deep copy)
        copy%next_var_id = this%next_var_id
        copy%subst = this%subst          ! Uses substitution_t assignment (deep copy)
    end function semantic_context_deep_copy

    subroutine semantic_context_assign(lhs, rhs)
        class(semantic_context_t), intent(out) :: lhs
        type(semantic_context_t), intent(in) :: rhs

        lhs%env = rhs%env                ! Uses type_env_t assignment (deep copy)
        lhs%scopes = rhs%scopes          ! Uses scope_stack_t assignment (deep copy)
        lhs%next_var_id = rhs%next_var_id
        lhs%subst = rhs%subst            ! Uses substitution_t assignment (deep copy)
    end subroutine semantic_context_assign

end module semantic_analyzer
