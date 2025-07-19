program test_nested_allocatable
    implicit none

    ! Mimic the type_var_t structure
    type :: type_var_t
        integer :: id
        character(len=:), allocatable :: name
    end type

    ! Mimic mono_type_t structure (recursive)
    type :: mono_type_t
        integer :: kind
        type(type_var_t) :: var
        type(mono_type_t), allocatable :: args(:)
        integer :: size
    end type

    ! Mimic poly_type_t structure
    type :: poly_type_t
        type(type_var_t), allocatable :: forall (:)
        type(mono_type_t) :: mono
    end type

    type(poly_type_t), allocatable :: result

    print *, "=== Test 1: Direct creation ==="
    allocate (result)
    result%mono%kind = 1
    result%mono%var%id = 1
    result%mono%var%name = "test"
    print *, "Created successfully"
    print *, "mono%var%name = ", result%mono%var%name

    deallocate (result)
    print *, "Deallocated successfully"

    print *, ""
    print *, "=== Test 2: Function return ==="
    result = create_poly_type()
    if (allocated(result)) then
        print *, "Result allocated"
        print *, "mono%kind = ", result%mono%kind
        print *, "mono%var%name = ", result%mono%var%name
    else
        print *, "Result not allocated"
    end if

contains

    function create_poly_type() result(pt)
        type(poly_type_t), allocatable :: pt

        print *, "Inside function, allocating..."
        allocate (pt)
        pt%mono%kind = 2
        pt%mono%var%id = 42
        pt%mono%var%name = "created"
        print *, "Allocation complete, returning..."
    end function

end program test_nested_allocatable
