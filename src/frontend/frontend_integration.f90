module frontend_integration
    ! Integration module for the compiler frontend
    ! Provides compatibility layer for existing tools
    
    use frontend, only: compile_source, compilation_options_t, BACKEND_FORTRAN
    use debug_state, only: get_debug_flags
    
    implicit none
    private
    
    public :: compile_with_frontend, compile_with_frontend_debug, is_simple_fortran_file
    
contains

    ! Main entry point (replaces preprocess_file)
    subroutine compile_with_frontend(input_file, output_file, error_msg)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg
        
        type(compilation_options_t) :: options
        
        ! Configure for Fortran backend (our current IR)
        options%backend = BACKEND_FORTRAN
        options%output_file = output_file
        
        ! Use global debug flags
        call get_debug_flags(options%debug_tokens, options%debug_ast, options%debug_codegen)
        
        ! Compile using the frontend
        call compile_source(input_file, options, error_msg)
        
    end subroutine compile_with_frontend
    
    ! Debug version (replaces preprocess_file_debug)
    subroutine compile_with_frontend_debug(input_file, output_file, error_msg, &
                                          debug_tokens, debug_ast, debug_codegen)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg
        logical, intent(in) :: debug_tokens, debug_ast, debug_codegen
        
        type(compilation_options_t) :: options
        
        ! Configure for Fortran backend with debug flags
        options%backend = BACKEND_FORTRAN
        options%output_file = output_file
        options%debug_tokens = debug_tokens
        options%debug_ast = debug_ast
        options%debug_codegen = debug_codegen
        
        ! Compile using the frontend
        call compile_source(input_file, options, error_msg)
        
    end subroutine compile_with_frontend_debug
    
    ! Check if file should use frontend (replaces is_preprocessor_file)
    function is_simple_fortran_file(filename) result(is_sf)
        character(len=*), intent(in) :: filename
        logical :: is_sf
        integer :: ext_pos
        
        ext_pos = index(filename, '.', back=.true.)
        if (ext_pos > 0) then
            is_sf = filename(ext_pos:) == '.f' .or. filename(ext_pos:) == '.F'
        else
            is_sf = .false.
        end if
    end function is_simple_fortran_file
    
end module frontend_integration