module notebook_output
    implicit none
    private

    ! Maximum number of output entries per cell
    integer, parameter :: MAX_OUTPUTS = 1000

    ! Output capture storage
    type, public :: output_entry_t
        character(len=:), allocatable :: content
        logical :: is_error = .false.
    contains
        procedure :: deep_copy => output_entry_deep_copy
        procedure :: assign => output_entry_assign
        generic :: assignment(=) => assign
    end type output_entry_t

    type, public :: cell_output_t
        type(output_entry_t), allocatable :: entries(:)
        integer :: count = 0
        logical :: has_error = .false.
    contains
        procedure :: deep_copy => cell_output_deep_copy
        procedure :: assign => cell_output_assign
        generic :: assignment(=) => assign
    end type cell_output_t

    ! Global output storage
    type(cell_output_t), allocatable :: cell_outputs(:)
    integer :: current_cell = 0
    integer :: max_cells = 100

    ! Public procedures
    public :: init_output_capture
    public :: start_cell_capture
    public :: notebook_print
    public :: notebook_error
    public :: get_cell_output
    public :: finalize_output_capture
    public :: write_outputs_to_file
    public :: read_outputs_from_file

contains

    subroutine init_output_capture(num_cells)
        integer, intent(in) :: num_cells
        integer :: i

        max_cells = num_cells
        if (allocated(cell_outputs)) deallocate (cell_outputs)
        allocate (cell_outputs(max_cells))

        do i = 1, max_cells
            allocate (cell_outputs(i)%entries(MAX_OUTPUTS))
            cell_outputs(i)%count = 0
            cell_outputs(i)%has_error = .false.
        end do

        current_cell = 0

    end subroutine init_output_capture

    subroutine start_cell_capture(cell_number)
        integer, intent(in) :: cell_number

        if (cell_number >= 1 .and. cell_number <= max_cells) then
            current_cell = cell_number
            cell_outputs(current_cell)%count = 0
            cell_outputs(current_cell)%has_error = .false.
        end if

    end subroutine start_cell_capture

    subroutine notebook_print(message)
        character(len=*), intent(in) :: message

        call add_output(message, .false.)

    end subroutine notebook_print

    subroutine notebook_print_int(label, value)
        character(len=*), intent(in) :: label
        integer, intent(in) :: value
        character(len=256) :: msg

        write (msg, '(a,i0)') trim(label)//' ', value
        call add_output(trim(msg), .false.)

    end subroutine notebook_print_int

    subroutine notebook_print_real(label, value)
        character(len=*), intent(in) :: label
        real(8), intent(in) :: value
        character(len=256) :: msg

        write (msg, '(a,f0.6)') trim(label)//' ', value
        call add_output(trim(msg), .false.)

    end subroutine notebook_print_real

    subroutine notebook_print_logical(label, value)
        character(len=*), intent(in) :: label
        logical, intent(in) :: value
        character(len=256) :: msg

        write (msg, '(a,l1)') trim(label)//' ', value
        call add_output(trim(msg), .false.)

    end subroutine notebook_print_logical

    subroutine notebook_error(message)
        character(len=*), intent(in) :: message

        call add_output(message, .true.)

    end subroutine notebook_error

    subroutine add_output(message, is_error)
        character(len=*), intent(in) :: message
        logical, intent(in) :: is_error

        if (current_cell >= 1 .and. current_cell <= max_cells) then
            if (cell_outputs(current_cell)%count < MAX_OUTPUTS) then
                cell_outputs(current_cell)%count = cell_outputs(current_cell)%count + 1
  cell_outputs(current_cell)%entries(cell_outputs(current_cell)%count)%content = message
cell_outputs(current_cell)%entries(cell_outputs(current_cell)%count)%is_error = is_error

                if (is_error) then
                    cell_outputs(current_cell)%has_error = .true.
                end if
            end if
        end if

    end subroutine add_output

    function get_cell_output(cell_number) result(output)
        integer, intent(in) :: cell_number
        type(cell_output_t) :: output
        integer :: i

        if (cell_number >= 1 .and. cell_number <= max_cells) then
            output = cell_outputs(cell_number)
        else
            ! Return empty output for invalid cell number
            allocate (output%entries(0))
            output%count = 0
            output%has_error = .false.
        end if

    end function get_cell_output

    subroutine finalize_output_capture()
        integer :: i, j

        if (allocated(cell_outputs)) then
            do i = 1, max_cells
                if (allocated(cell_outputs(i)%entries)) then
                    do j = 1, cell_outputs(i)%count
                        if (allocated(cell_outputs(i)%entries(j)%content)) then
                            deallocate (cell_outputs(i)%entries(j)%content)
                        end if
                    end do
                    deallocate (cell_outputs(i)%entries)
                end if
            end do
            deallocate (cell_outputs)
        end if

    end subroutine finalize_output_capture

    subroutine write_outputs_to_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit, i, j

        open (newunit=unit, file=filename, status='replace')

        ! Write number of cells
        write (unit, '(i0)') max_cells

        ! Write each cell's output
        do i = 1, max_cells
            write (unit, '(i0,1x,l1)') cell_outputs(i)%count, cell_outputs(i)%has_error
            do j = 1, cell_outputs(i)%count
                ! Write with a delimiter to avoid space issues
            write (unit, '(l1,1x,">>>",a,"<<<")') cell_outputs(i)%entries(j)%is_error, &
                    trim(cell_outputs(i)%entries(j)%content)
            end do
        end do

        close (unit)

    end subroutine write_outputs_to_file

    subroutine read_outputs_from_file(filename, cell_results)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: cell_results(:)

        integer :: unit, i, j, num_cells, entry_count, content_len, start_pos, end_pos
        logical :: has_error, is_error
        character(len=1024) :: line, content, full_line

        open (newunit=unit, file=filename, status='old')

        ! Read number of cells
        read (unit, *) num_cells
        allocate (character(len=1024) :: cell_results(num_cells))

        ! Read each cell's output
        do i = 1, num_cells
            read (unit, *) entry_count, has_error
            cell_results(i) = ""

            do j = 1, entry_count
                read (unit, '(a)') full_line

                ! Parse the line: logical>>>content<<<
                if (full_line(1:1) == 'T') then
                    is_error = .true.
                else
                    is_error = .false.
                end if

                start_pos = index(full_line, '>>>') + 3
                end_pos = index(full_line, '<<<') - 1

                if (start_pos > 3 .and. end_pos > start_pos) then
                    content = full_line(start_pos:end_pos)
                else
                    content = ""
                end if
                if (len_trim(cell_results(i)) > 0) then
                   cell_results(i) = trim(cell_results(i))//new_line('a')//trim(content)
                else
                    cell_results(i) = trim(content)
                end if
            end do
        end do

        close (unit)

    end subroutine read_outputs_from_file

    ! Deep copy procedures for output_entry_t
    function output_entry_deep_copy(this) result(copy)
        class(output_entry_t), intent(in) :: this
        type(output_entry_t) :: copy

        copy%is_error = this%is_error
        if (allocated(this%content)) then
            copy%content = this%content
        end if
    end function output_entry_deep_copy

    subroutine output_entry_assign(lhs, rhs)
        class(output_entry_t), intent(out) :: lhs
        type(output_entry_t), intent(in) :: rhs

        lhs%is_error = rhs%is_error
        if (allocated(rhs%content)) then
            lhs%content = rhs%content
        end if
    end subroutine output_entry_assign

    ! Deep copy procedures for cell_output_t
    function cell_output_deep_copy(this) result(copy)
        class(cell_output_t), intent(in) :: this
        type(cell_output_t) :: copy
        integer :: i

        copy%count = this%count
        copy%has_error = this%has_error

        if (allocated(this%entries)) then
            allocate (copy%entries(size(this%entries)))
            do i = 1, size(this%entries)
                copy%entries(i) = this%entries(i)  ! Uses output_entry_t assignment (deep copy)
            end do
        end if
    end function cell_output_deep_copy

    subroutine cell_output_assign(lhs, rhs)
        class(cell_output_t), intent(out) :: lhs
        type(cell_output_t), intent(in) :: rhs
        integer :: i

        lhs%count = rhs%count
        lhs%has_error = rhs%has_error

        if (allocated(rhs%entries)) then
            allocate (lhs%entries(size(rhs%entries)))
            do i = 1, size(rhs%entries)
                lhs%entries(i) = rhs%entries(i)  ! Uses output_entry_t assignment (deep copy)
            end do
        end if
    end subroutine cell_output_assign

end module notebook_output
