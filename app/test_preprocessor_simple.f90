program test_standardizer_simple
    use standardizer, only: standardize_file
    use temp_utils, only: create_temp_dir, get_temp_file_path
    implicit none

    character(len=256) :: error_msg

    call standardize_file(get_temp_file_path(create_temp_dir('fortran_test'), 'test_standardize.f'), get_temp_file_path(create_temp_dir('fortran_test'), 'test_standardize.f90'), error_msg)

    if (len_trim(error_msg) > 0) then
        print *, 'Error: ', trim(error_msg)
    else
        print *, 'Success - check '//get_temp_file_path(create_temp_dir('fortran_test'), 'test_standardize.f90')
    end if
end program
