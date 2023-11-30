! Задание 8: Гауссова квадратура
! Выполнено до пункта 5, коэффициенты формулы Гаусса считаются неверно
! Передача аргумента через make mode=N

program quest8v2
    use my_io
    use my_math
    use my_consts
    implicit none
    
    integer :: n, i
    real(mp), allocatable :: t(:), a(:)

    call read_argument(1, n, default=4)

    t = legendre_polynomial_roots(n)
    call output('Корни полинома Лежандра:', t)

    a = gaussian_quadrature_coefficients(t)
    call output('Коэффициенты формулы Гаусса:', a)

    open(1, file='quad'//str(n)//'.dat')
        do i=1,n
            write(1,'(2f9.'//str(dp)//')') a(i), t(i)
        end do
    close(1)
    
end program