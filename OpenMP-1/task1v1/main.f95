module functions
    use my_io
    implicit none
    contains

    function f(x) result(y)
        real(mp) :: x, y
        y = x**2 - 1
    end function

end module

program openmp1_1v1
    use my_io
    use my_math
    use functions
    implicit none

    integer :: n
    real(mp) :: a, b

    a = 1
    b = 10
    n = 100

    write(*,*) integrate(f, a, b, n, 'rectangle') !324

end program