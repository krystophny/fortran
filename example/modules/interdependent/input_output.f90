module input_output
    use constants, only: pi
    implicit none

    public :: print_results

contains

    subroutine print_results(radius, height, area, volume)
        real(8), intent(in) :: radius, height, area, volume

        print '(a)', '=== Cylinder Calculations ==='
        print '(a,f8.2)', 'Radius: ', radius
        print '(a,f8.2)', 'Height: ', height
        print '(a,f8.6)', 'π value: ', pi
        print '(a,f12.4)', 'Base area: ', area
        print '(a,f12.4)', 'Volume: ', volume
        print '(a)', '============================'

    end subroutine print_results

end module input_output
