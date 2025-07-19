program test_minimal_alloc_bug
    implicit none

    type :: my_type
        integer :: x
    end type

    type(my_type), allocatable :: result

    print *, "Test 1: Returning allocated"
    call get_allocated(result)
    print *, "Success! allocated = ", allocated(result)

    print *, ""
    print *, "Test 2: Returning unallocated"
    if (allocated(result)) deallocate (result)
    call get_unallocated(result)
    print *, "Success! allocated = ", allocated(result)

contains

    subroutine get_allocated(mt)
        type(my_type), allocatable, intent(out) :: mt
        allocate (mt)
        mt%x = 42
    end subroutine

    subroutine get_unallocated(mt)
        type(my_type), allocatable, intent(out) :: mt
        ! Return without allocating - mt is automatically deallocated on entry
    end subroutine

end program test_minimal_alloc_bug
