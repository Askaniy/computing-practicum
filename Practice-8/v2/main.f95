! Задание 8: Гауссова квадратура
! Пункт 5: сохранение корней полинома Лежандра и коэффициентов полинома Гаусса в отдельный файл
! Передача аргумента через make mode=N

program quest8v2p5
    use my_io
    use my_math
    implicit none
    
    integer :: n, i
    real(mp), allocatable :: t(:), a(:)

    call read_argument(1, n, default=5)

    t = legendre_polynomial_roots(n)
    call output('Корни полинома Лежандра:', t)

    a = gaussian_quadrature_coefficients(t)
    call output('Коэффициенты формулы Гаусса:', a)

    open(1, file='quad'//zfill(n, 2)//'.dat')
        do i=1,n
            write(1,'(2f9.'//str(dp)//')') a(i), t(i)
        end do
    close(1)
    
end program