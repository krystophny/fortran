module notebook_parser
    implicit none
    private

    ! Public constants
    integer, parameter, public :: CELL_CODE = 1
    integer, parameter, public :: CELL_MARKDOWN = 2

    ! Maximum number of cells
    integer, parameter :: MAX_CELLS = 1000

    ! Cell type
    type, public :: cell_t
        integer :: cell_type = CELL_CODE
        character(len=:), allocatable :: content
        integer :: start_line = 0
        integer :: end_line = 0
    contains
        procedure :: deep_copy => cell_deep_copy
        procedure :: assign => cell_assign
        generic :: assignment(=) => assign
    end type cell_t

    ! Notebook type
    type, public :: notebook_t
        type(cell_t), allocatable :: cells(:)
        integer :: num_cells = 0
        character(len=:), allocatable :: source_file
    contains
        procedure :: deep_copy => notebook_deep_copy
        procedure :: assign => notebook_assign
        generic :: assignment(=) => assign
    end type notebook_t

    ! Public procedures
    public :: parse_notebook
    public :: parse_notebook_file
    public :: free_notebook

contains

    subroutine parse_notebook(content, notebook)
        character(len=*), intent(in) :: content
        type(notebook_t), intent(out) :: notebook

        character(len=:), allocatable :: lines(:)
        integer :: num_lines, i, current_cell
        integer :: cell_type
        logical :: in_cell
        integer :: cell_start
        character(len=:), allocatable :: cell_content

        ! Initialize notebook
        allocate (notebook%cells(MAX_CELLS))
        notebook%num_cells = 0

        ! Split content into lines
        call split_lines(content, lines, num_lines)

        ! Initialize parsing state
        in_cell = .false.
        current_cell = 0
        cell_type = CELL_CODE  ! Default to code cell
        cell_start = 1

        ! Parse lines
        do i = 1, num_lines
            if (is_cell_marker(lines(i))) then
                ! Save previous cell if exists
                if (in_cell .and. current_cell > 0) then
                    call save_cell_content(notebook%cells(current_cell), &
                                           lines, cell_start, i - 1)
                end if

                ! Start new cell
                cell_type = get_cell_type(lines(i))
                current_cell = current_cell + 1
                notebook%num_cells = current_cell
                notebook%cells(current_cell)%cell_type = cell_type
                notebook%cells(current_cell)%start_line = i
                cell_start = i + 1
                in_cell = .true.
            end if
        end do

        ! Save last cell
        if (in_cell .and. current_cell > 0) then
            call save_cell_content(notebook%cells(current_cell), &
                                   lines, cell_start, num_lines)
        else if (current_cell == 0 .and. num_lines > 0) then
            ! No markers found, entire content is one code cell
            current_cell = 1
            notebook%num_cells = 1
            notebook%cells(1)%cell_type = CELL_CODE
            notebook%cells(1)%start_line = 1
            call save_cell_content(notebook%cells(1), lines, 1, num_lines)
        end if

        ! Resize cells array to actual size
        if (notebook%num_cells > 0) then
            notebook%cells = notebook%cells(1:notebook%num_cells)
        else
            deallocate (notebook%cells)
            allocate (notebook%cells(0))
        end if

    end subroutine parse_notebook

    subroutine parse_notebook_file(filename, notebook)
        character(len=*), intent(in) :: filename
        type(notebook_t), intent(out) :: notebook

        character(len=:), allocatable :: content
        integer :: unit, iostat
        integer :: file_size

        ! Read entire file
        call read_file(filename, content)

        ! Parse content
        call parse_notebook(content, notebook)

        ! Store source filename
        notebook%source_file = filename

    end subroutine parse_notebook_file

    subroutine free_notebook(notebook)
        type(notebook_t), intent(inout) :: notebook
        integer :: i

        if (allocated(notebook%cells)) then
            do i = 1, notebook%num_cells
                if (allocated(notebook%cells(i)%content)) then
                    deallocate (notebook%cells(i)%content)
                end if
            end do
            deallocate (notebook%cells)
        end if

        if (allocated(notebook%source_file)) then
            deallocate (notebook%source_file)
        end if

        notebook%num_cells = 0

    end subroutine free_notebook

    ! Helper functions

    function is_cell_marker(line) result(is_marker)
        character(len=*), intent(in) :: line
        logical :: is_marker
        character(len=:), allocatable :: trimmed

        trimmed = adjustl(line)

        ! Check for cell markers at start of line
        is_marker = .false.
        if (len_trim(trimmed) >= 4) then
            if (trimmed(1:4) == "! %%") then
                ! Make sure it's at the beginning of a line, not inside code
                is_marker = .true.
            end if
        end if

    end function is_cell_marker

    function get_cell_type(line) result(cell_type)
        character(len=*), intent(in) :: line
        integer :: cell_type
        character(len=:), allocatable :: trimmed

        trimmed = adjustl(line)
        cell_type = CELL_CODE  ! Default

        ! Check for markdown marker
        if (index(trimmed, "[markdown]") > 0) then
            cell_type = CELL_MARKDOWN
        end if

    end function get_cell_type

    subroutine save_cell_content(cell, lines, start_line, end_line)
        type(cell_t), intent(inout) :: cell
        character(len=:), allocatable, intent(in) :: lines(:)
        integer, intent(in) :: start_line, end_line

        integer :: i, content_length
        character(len=:), allocatable :: temp_content
        logical :: first_line

        ! Calculate total content length
        content_length = 0
        do i = start_line, end_line
            content_length = content_length + len_trim(lines(i)) + 1  ! +1 for newline
        end do

        if (content_length == 0) then
            cell%content = ""
            return
        end if

        ! Build content
        allocate (character(len=content_length) :: cell%content)
        cell%content = ""
        first_line = .true.

        do i = start_line, end_line
            if (.not. first_line) then
                cell%content = trim(cell%content)//new_line('a')
            else
                first_line = .false.
            end if

            ! For markdown cells, remove comment markers
            if (cell%cell_type == CELL_MARKDOWN) then
                block
                    character(len=:), allocatable :: cleaned_line
                    call remove_comment_marker(lines(i), cleaned_line)
                    cell%content = trim(cell%content)//trim(cleaned_line)
                end block
            else
                cell%content = trim(cell%content)//trim(lines(i))
            end if
        end do

        cell%end_line = end_line

    end subroutine save_cell_content

    subroutine remove_comment_marker(line, cleaned)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: cleaned
        character(len=:), allocatable :: trimmed

        trimmed = adjustl(line)

        ! Remove leading "!" and spaces
        if (len_trim(trimmed) > 0 .and. trimmed(1:1) == "!") then
            if (len_trim(trimmed) > 1) then
                cleaned = adjustl(trimmed(2:))
            else
                cleaned = ""
            end if
        else
            cleaned = trimmed
        end if

    end subroutine remove_comment_marker

    subroutine split_lines(content, lines, num_lines)
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: num_lines

        integer :: i, line_start, line_count
        integer :: max_line_length

        ! Count lines and find max length
        line_count = 1
        max_line_length = 0
        line_start = 1

        do i = 1, len(content)
            if (content(i:i) == new_line('a')) then
                max_line_length = max(max_line_length, i - line_start)
                line_count = line_count + 1
                line_start = i + 1
            end if
        end do
        ! Last line
        max_line_length = max(max_line_length, len(content) - line_start + 1)

        ! Allocate lines array
        allocate (character(len=max_line_length) :: lines(line_count))

        ! Split content into lines
        line_count = 1
        line_start = 1
        do i = 1, len(content)
            if (content(i:i) == new_line('a')) then
                lines(line_count) = content(line_start:i - 1)
                line_count = line_count + 1
                line_start = i + 1
            end if
        end do
        ! Last line
        if (line_start <= len(content)) then
            lines(line_count) = content(line_start:)
        else
            lines(line_count) = ""
        end if

        num_lines = line_count

    end subroutine split_lines

    subroutine read_file(filename, content)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: content

        integer :: unit, iostat, file_size
        character(len=1) :: buffer

        open (newunit=unit, file=filename, status='old', &
              access='stream', form='unformatted', iostat=iostat)

        if (iostat /= 0) then
            content = ""
            return
        end if

        ! Get file size
        inquire (unit=unit, size=file_size)

        ! Allocate content
        allocate (character(len=file_size) :: content)

        ! Read entire file
        read (unit, iostat=iostat) content

        close (unit)

    end subroutine read_file

    ! Deep copy procedures for cell_t
    function cell_deep_copy(this) result(copy)
        class(cell_t), intent(in) :: this
        type(cell_t) :: copy

        copy%cell_type = this%cell_type
        copy%start_line = this%start_line
        copy%end_line = this%end_line

        if (allocated(this%content)) then
            copy%content = this%content
        end if
    end function cell_deep_copy

    subroutine cell_assign(lhs, rhs)
        class(cell_t), intent(out) :: lhs
        type(cell_t), intent(in) :: rhs

        lhs%cell_type = rhs%cell_type
        lhs%start_line = rhs%start_line
        lhs%end_line = rhs%end_line

        if (allocated(rhs%content)) then
            lhs%content = rhs%content
        end if
    end subroutine cell_assign

    ! Deep copy procedures for notebook_t
    function notebook_deep_copy(this) result(copy)
        class(notebook_t), intent(in) :: this
        type(notebook_t) :: copy
        integer :: i

        copy%num_cells = this%num_cells

        if (allocated(this%cells)) then
            allocate (copy%cells(size(this%cells)))
            do i = 1, size(this%cells)
                copy%cells(i) = this%cells(i)  ! Uses cell_t assignment (deep copy)
            end do
        end if

        if (allocated(this%source_file)) then
            copy%source_file = this%source_file
        end if
    end function notebook_deep_copy

    subroutine notebook_assign(lhs, rhs)
        class(notebook_t), intent(out) :: lhs
        type(notebook_t), intent(in) :: rhs
        integer :: i

        lhs%num_cells = rhs%num_cells

        if (allocated(rhs%cells)) then
            allocate (lhs%cells(size(rhs%cells)))
            do i = 1, size(rhs%cells)
                lhs%cells(i) = rhs%cells(i)  ! Uses cell_t assignment (deep copy)
            end do
        end if

        if (allocated(rhs%source_file)) then
            lhs%source_file = rhs%source_file
        end if
    end subroutine notebook_assign

end module notebook_parser
