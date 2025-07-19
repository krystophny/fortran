! derived_types.f90 - Standard Fortran derived types
program derived_types
    implicit none

    type :: person_type
        character(len=20) :: name
        integer :: age
        real(8) :: height
    end type person_type

    type :: car_type
        character(len=20) :: make
        integer :: year
        real(8) :: price
    end type car_type

    type(person_type) :: person
    type(car_type) :: car

    person%name = "Alice"
    person%age = 30
    person%height = 5.6_8

    car%make = "Toyota"
    car%year = 2020
    car%price = 25000.0_8

    print *, 'Person name:', person%name
    print *, 'Person age:', person%age
    print *, 'Person height:', person%height

    print *, 'Car make:', car%make
    print *, 'Car year:', car%year
    print *, 'Car price:', car%price
end program derived_types
