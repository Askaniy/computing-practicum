module sandbox
    implicit none
    real(4) :: pi = 3.14159
    !complex(4) :: i = (0, 1)

    contains

    function fast_fourier_transform(input) result(array1)
        real(4), intent(in) :: input(:)
        real(4), allocatable :: array0(:), array1(:)
        integer :: n, m

        ! Вычисление количества шагов
        m = ceiling(log(real(size(input)))/log(2.))

        ! Расширение нулями входного массива до ближайшей степени двойки
        n = 2**m
        allocate(array0(n))
        allocate(array1(n))
        array0 = 0
        array0(:size(input)) = input
        
    end function

    ! Рекурсивная часть преобразования
    function recursive_FFT(array0) result(array1)
        real(4), intent(in) :: array0(:)
        real(4) :: array1(size(array0))
    end function

    ! Дискретное преобразование Фурье
    function discrete_fourier_transform(array0, sign) result(array1)
        real(4), intent(in) :: array0(:)
        integer, intent(in) :: sign
        real(4) :: array1(size(array0)), nk(size(array0))
        complex(4) :: w(size(array0), size(array0))
        integer :: m, i
        m = size(array0)
        nk = [(i, i=0,m-1)]
        w = exp(sign * complex(0, 2) * pi / m * meshgrid(nk, nk))
        array1 = matmul(array0, w) / sqrt(real(m))
    end function

    ! Создаёт матрицу из попарных перемножений
    function meshgrid(x, y) result(grid)
        real(4), intent(in), dimension(:) :: x, y
        real(4), dimension(size(x), size(y)) :: grid
        integer :: i, j
        do concurrent (i=1:size(x), j=1:size(y))
            grid(i,j) = x(i) * y(j)
        end do
    end function

end module

program main
    use sandbox
    implicit none

    real(4) :: array0(10)=[1, 2, 2, 1, 1, 2, 3, 1, 1, 2]
    real(4), allocatable :: array1(:)

    array1 = discrete_fourier_transform(discrete_fourier_transform(array0, -1), 1)
    write(*,*) array1 ! output =  1.00001633       2.00000715       1.49999762       1.00000191       1.99999642       1.99998522       1.99998844      0.999996901       1.50000155       2.00000525
    
end program