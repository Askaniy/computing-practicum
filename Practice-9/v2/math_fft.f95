
! Модуль реализации быстрого преобразования Фурье

module math_fft
    use io_precision, only: mp
    use math_general, only: pi, meshgrid
    implicit none

    private
    public fast_fourier_transform
    
    integer, private :: i

    contains

    ! Быстрое преобразование Фурье
    ! Может быть ускорено в два раза, если предрассчитывать коэффициенты w
    function fast_fourier_transform(array, sign) result(array1)
        complex(mp), intent(in) :: array(:)
        integer, intent(in) :: sign
        complex(mp), allocatable :: array0(:), array1(:)
        integer :: n
        ! Расширение нулями входного массива до ближайшей степени двойки
        n = 2**ceiling(log(real(size(array)))/log(2.))
        allocate(array0(0:n-1))
        allocate(array1(0:n-1))
        array0 = 0
        array0(:size(array)-1) = array
        array1 = recursive_FFT(array0, sign) / sqrt(real(n))
    end function

    ! Рекурсивная часть быстрого преобразования Фурье
    recursive function recursive_FFT(array0, sign) result(array1)
        complex(mp), intent(in) :: array0(0:)
        integer, intent(in) :: sign
        complex(mp) :: array1(0:size(array0)-1), w(size(array0)/2)
        integer :: n
        n = size(array0)
        if (n == 2) then
            array1(0) = array0(0) + array0(1)
            array1(1) = array0(0) - array0(1)
        else
            w = exp(sign * cmplx(0, 2) * pi / n * [(i, i=0,n/2-1)])
            array1(0::2) = recursive_FFT(array0(:n/2-1) + array0(n/2:), sign)
            array1(1::2) = recursive_FFT(w * (array0(:n/2-1) - array0(n/2:)), sign)
        end if
    end function

    ! Дискретное преобразование Фурье, эталон для тестирования
    function discrete_fourier_transform(array0, sign) result(array1)
        complex(mp), intent(in) :: array0(:)
        integer, intent(in) :: sign
        complex(mp) :: array1(size(array0)), w(size(array0), size(array0))
        integer :: n, nk(size(array0))
        n = size(array0)
        nk = [(i, i=0,n-1)]
        w = exp(sign * cmplx(0, 2) * pi / n * meshgrid(nk, nk))
        array1 = matmul(array0, w) / sqrt(real(n))
    end function

end module