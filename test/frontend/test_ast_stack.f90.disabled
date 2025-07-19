program test_ast_stack
    use ast_core
    implicit none

    ! Simple visitor for testing
    type :: test_visitor
        integer :: count = 0
    end type test_visitor

    ! Test the new stack-based AST system
    call test_basic_stack_operations()
    call test_depth_traversal()
    call test_performance()
    call test_memory_safety()

    print *, "All stack-based AST tests passed!"

contains

    subroutine test_basic_stack_operations()
        type(ast_stack_t) :: stack
        type(identifier_node) :: id1, id2
        type(literal_node) :: lit1
        type(assignment_node) :: assign1
        class(ast_node), allocatable :: current_node
        type(ast_stack_stats_t) :: stats

        print *, "Testing basic stack operations..."

        ! Create test nodes
        id1 = create_identifier("x", 1, 1)
        id2 = create_identifier("y", 1, 5)
        lit1 = create_literal("42", LITERAL_INTEGER, 1, 9)

        ! Create stack and test push operations
        stack = create_ast_stack(10)

        ! Push nodes and verify depth tracking
        call stack%push(id1, "identifier")
        if (stack%get_depth() /= 0) error stop "Root should have depth 0"

        call stack%push(lit1, "literal", 1)  ! Parent is index 1
        if (stack%get_depth() /= 1) error stop "Child should have depth 1"

        call stack%push(id2, "identifier", 1)  ! Parent is index 1
        if (stack%get_depth() /= 1) error stop "Child should have depth 1"

        ! Test current node retrieval
        current_node = stack%current()
        if (.not. allocated(current_node)) error stop "Current node should be allocated"

        ! Test pop operations
        call stack%pop()
        if (stack%get_depth() /= 1) error stop "After pop, depth should be 1"

        call stack%pop()
        if (stack%get_depth() /= 0) error stop "After pop, depth should be 0"

        ! Test statistics
        stats = stack%get_stats()
        if (stats%total_nodes /= 1) error stop "Should have 1 node remaining"
        if (stats%max_depth /= 1) error stop "Max depth should be 1"

        print *, "  ✓ Basic stack operations working correctly"
    end subroutine test_basic_stack_operations

    subroutine test_depth_traversal()
        type(ast_stack_t) :: stack
        type(identifier_node) :: id1, id2, id3
        type(literal_node) :: lit1, lit2
        type(assignment_node) :: assign1, assign2
        type(program_node) :: prog
        type(test_visitor) :: visitor
        integer, allocatable :: indices(:)

        print *, "Testing depth traversal..."

        ! Create nodes
        id1 = create_identifier("x", 1, 1)
        id2 = create_identifier("y", 2, 1)
        id3 = create_identifier("z", 3, 1)
        lit1 = create_literal("42", LITERAL_INTEGER, 1, 5)
        lit2 = create_literal("3.14", LITERAL_REAL, 2, 5)

        ! Create stack and build tree structure
        stack = create_ast_stack(20)

        ! Push program (depth 0)
        prog = create_program("test", [integer ::])
        call stack%push(prog, "program")

        ! Push assignments (depth 1)
        assign1 = create_assignment(2, 3)  ! Will be corrected after nodes are added
        assign2 = create_assignment(4, 5)  ! Will be corrected after nodes are added
        call stack%push(assign1, "assignment", 1)
        call stack%push(assign2, "assignment", 1)

        ! Push identifiers and literals (depth 2)
        call stack%push(id1, "identifier", 2)
        call stack%push(id2, "identifier", 3)
        call stack%push(lit1, "literal", 2)
        call stack%push(lit2, "literal", 3)

        ! Test finding nodes by type
        indices = stack%find_by_type("assignment")
        if (size(indices) /= 2) error stop "Should find 2 assignments"

        indices = stack%find_by_type("identifier")
        if (size(indices) /= 2) error stop "Should find 2 identifiers"

        indices = stack%find_by_type("literal")
        if (size(indices) /= 2) error stop "Should find 2 literals"

        ! Test depth traversal
        visitor%count = 0
        call stack%traverse_depth(0, visitor)
        if (visitor%count /= 1) error stop "Should visit 1 node at depth 0"

        visitor%count = 0
        call stack%traverse_depth(1, visitor)
        if (visitor%count /= 2) error stop "Should visit 2 nodes at depth 1"

        visitor%count = 0
        call stack%traverse_depth(2, visitor)
        if (visitor%count /= 4) error stop "Should visit 4 nodes at depth 2"

        print *, "  ✓ Depth traversal working correctly"
    end subroutine test_depth_traversal

    subroutine test_performance()
        type(ast_stack_t) :: stack
        type(identifier_node) :: id_node
        type(ast_stack_stats_t) :: stats
        integer :: i, parent_index

        print *, "Testing performance with large trees..."

        ! Create stack with large capacity
        stack = create_ast_stack(1000)

        ! Add many nodes in a chain
        parent_index = 0
        do i = 1, 500
            block
                character(len=10) :: name_str
                write (name_str, '(A,I0)') "var", i
                id_node = create_identifier(trim(name_str), i, 1)
                call stack%push(id_node, "identifier", parent_index)
                parent_index = i  ! Each node becomes parent of next
            end block
        end do

        ! Verify performance characteristics
        stats = stack%get_stats()
        if (stats%total_nodes /= 500) error stop "Should have 500 nodes"
        if (stats%max_depth /= 499) error stop "Max depth should be 499"

        ! Test traversal at various depths
        block
            type(test_visitor) :: visitor
            visitor%count = 0
            call stack%traverse_depth(250, visitor)
            if (visitor%count /= 1) error stop "Should visit 1 node at depth 250"
        end block

        print *, "  ✓ Performance test passed (500 nodes)"
    end subroutine test_performance

    subroutine test_memory_safety()
        type(ast_stack_t) :: stack
        type(identifier_node) :: id_node
        class(ast_node), allocatable :: retrieved_node

        print *, "Testing memory safety..."

        ! Test with small initial capacity to force reallocation
        stack = create_ast_stack(2)

        ! Add nodes beyond initial capacity
        id_node = create_identifier("a", 1, 1)
        call stack%push(id_node, "identifier")

        id_node = create_identifier("b", 1, 1)
        call stack%push(id_node, "identifier")

        id_node = create_identifier("c", 1, 1)
        call stack%push(id_node, "identifier")  ! Should trigger reallocation

        ! Verify stack still works after reallocation
        retrieved_node = stack%current()
        if (.not. allocated(retrieved_node)) error stop "Node should be retrievable after reallocation"

        ! Test clear operation
        call stack%clear()
        block
            type(ast_stack_stats_t) :: stats
            stats = stack%get_stats()
            if (stats%total_nodes /= 0) error stop "Stack should be empty after clear"
        end block

        print *, "  ✓ Memory safety test passed"
    end subroutine test_memory_safety

end program test_ast_stack
