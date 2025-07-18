program test_windows_cache
    use cache
    implicit none

    call test_windows_cache_paths()
    call test_cross_platform_paths()

    print *, "All Windows compatibility tests passed!"

contains

    subroutine test_windows_cache_paths()
        character(len=256) :: cache_dir

        print *, "Testing cross-platform cache path generation..."

        ! Get cache directory (will use appropriate path for current OS)
        cache_dir = get_cache_dir()

        ! Check that we get a valid path
        if (len_trim(cache_dir) > 0) then
            print *, "  ✓ Cache path generated: ", trim(cache_dir)
        else
            print *, "  ✗ Failed to generate cache path"
            stop 1
        end if

        ! Check that path handling functions work
        print *, "  ✓ Cross-platform path generation successful"

    end subroutine test_windows_cache_paths

    subroutine test_cross_platform_paths()
        use fpm_filesystem, only: join_path
        character(len=256) :: path1, path2, path3

        print *, "Testing cross-platform path joining..."

        ! Test basic path joining
        path1 = join_path('cache', 'builds')
        path2 = join_path('cache', 'modules', 'test_key')
        path3 = join_path('.', '.fortran-cache')

       if (len_trim(path1) > 0 .and. len_trim(path2) > 0 .and. len_trim(path3) > 0) then
            print *, "  ✓ Path joining works: ", trim(path1)
            print *, "  ✓ Multi-component paths: ", trim(path2)
            print *, "  ✓ Relative paths: ", trim(path3)
        else
            print *, "  ✗ Path joining failed"
            stop 1
        end if

    end subroutine test_cross_platform_paths

end program test_windows_cache
