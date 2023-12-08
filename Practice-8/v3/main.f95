! Задание 8: Гауссова квадратура

module functions
    use my_io
    implicit none

    contains

    pure function f1(x) result(y)
        real(mp), intent(in) :: x
        real(mp) :: y
        y = x*x
    end function

end module

program quest8v2p6
    use my_io
    use my_math
    use functions
    implicit none
    
    real(mp) :: a, b, integral
    integer :: n

    a = 0
    b = 3
    n = 10
    
    integral = integrate(f1, a, b, n, 's')
    call output('интеграл =', integral)
    
end program