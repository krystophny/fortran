! control_flow.f90 - Standard Fortran control flow examples
program control_flow
    implicit none
    real(8) :: temperature
    integer :: grade, i, fib1, fib2, fib_next, n, day_num, num, divisor
    logical :: is_passed, is_prime

    ! Initialize test variables
    temperature = 25.5_8
    grade = 85
    is_passed = .false.

    print *, "Temperature:", temperature, "°C"
    print *, "Grade:", grade

    ! Temperature classification
    if (temperature < 0.0_8) then
        print *, "It's freezing!"
    else if (temperature < 20.0_8) then
        print *, "It's cold"
    else if (temperature < 30.0_8) then
        print *, "It's mild"
    else
        print *, "It's hot!"
    end if

    ! Grade evaluation
    if (grade >= 60) then
        is_passed = .true.
        print *, "Student passed with grade:", grade
    else
        is_passed = .false.
        print *, "Student failed with grade:", grade
    end if

    ! Complex logical condition
    if (grade >= 90 .and. is_passed) then
        print *, "Excellent work!"
    else if (grade >= 80 .and. is_passed) then
        print *, "Good job!"
    else if (is_passed) then
        print *, "Keep trying!"
    else
        print *, "Need more study"
    end if

    ! Simple do loop
    print *, "Counting from 1 to 5:"
    do i = 1, 5
        print *, "Count:", i
    end do

    ! Fibonacci sequence (first 10 terms)
    fib1 = 0
    fib2 = 1
    n = 0

    print *, "Fibonacci sequence:"
    print *, "F(0) =", fib1
    print *, "F(1) =", fib2

    do while (n < 8)
        fib_next = fib1 + fib2
        n = n + 1
        print *, "F(", n + 1, ") =", fib_next
        fib1 = fib2
        fib2 = fib_next
    end do

    ! Day of week classification
    day_num = 3

    select case (day_num)
    case (1)
        print *, "Monday - Start of work week"
    case (2:5)
        print *, "Weekday - Work day"
    case (6)
        print *, "Saturday - Weekend starts"
    case (7)
        print *, "Sunday - Day of rest"
    case default
        print *, "Invalid day number"
    end select

    ! Find prime numbers up to 20
    print *, "Prime numbers up to 20:"

    outer: do num = 2, 20
        is_prime = .true.

        ! Check for divisors
        inner: do divisor = 2, int(sqrt(real(num)))
            if (mod(num, divisor) == 0) then
                is_prime = .false.
                exit inner
            end if
        end do inner

        if (is_prime) then
            print *, num, "is prime"
        end if
    end do outer

end program control_flow
