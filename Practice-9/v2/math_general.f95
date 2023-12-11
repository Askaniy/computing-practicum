
! Модуль основных математических процедур

module math_general
    use io_precision, only: mp
    implicit none

    private
    public factorial, mean, length, dist, diffs, apply, swap, find_index, meshgrid, multiply
    
    real(mp), parameter, public :: pi = 4*atan(1.0_mp)
    real(mp), parameter, public :: eps = epsilon(0._mp) ! Точность вычислений
    real(mp), parameter, public :: sqrt_eps = sqrt(eps) ! Шаг дифференцирования
    integer, private :: i, j, k

    interface swap
        module procedure swap_int, swap_real
    end interface
    
    interface meshgrid
        module procedure meshgrid_int, meshgrid_real
    end interface
    
    interface multiply
        module procedure multiply_1D_1D, multiply_1D_2D, multiply_2D_1D, multiply_2D_2D
    end interface

    contains

    ! Возвращает факториал
    pure integer function factorial(n)
        integer, intent(in) :: n
        factorial = product([(i, i=1,n)])
    end function

    ! Возвращает среднее арифметическое
    pure function mean(a)
        real(mp), intent(in) :: a(:)
        real(mp) :: mean
        mean = sum(a) / size(a)
    end function

    ! Возвращает длину вектора
    pure function length(a)
        real(mp), intent(in) :: a(:)
        real(mp) :: length
        length = sqrt(sum(a**2))
    end function

    ! Возвращает евклидову метрику
    pure function dist(a, b)
        real(mp), intent(in) :: a(:), b(:)
        real(mp) :: dist
        dist = length(b - a)
    end function

    ! Создание массива промежутков между элементами
    pure function diffs(array)
        real(mp), intent(in) :: array(:)
        real(mp) :: diffs(size(array)-1)
        diffs = array(2:size(array)) - array(1:size(array)-1)
    end function

    ! Аналог функции map в Python. Для случаев, когда нельзя использовать elemental
    pure function apply(f, array0) result(array1)
        real(mp), intent(in) :: array0(:)
        real(mp) :: array1(size(array0))
        interface
            pure function f(x) result(y)
                use io_precision, only: mp
                real(mp), intent(in) :: x
                real(mp) :: y
            end function
        end interface
        do concurrent (i=1:size(array0))
            array1(i) = f(array0(i))
        end do
    end function

    ! Бинарный поиск нижнего индекса по сортированному массиву
    pure function find_index(array, t) result(left)
        real(mp), intent(in) :: array(:), t
        integer :: left, right, mid
        left = 1 ! индексация ответа от единицы
        right = size(array)
        do while (left < right-1)
            mid = (left + right) / 2
            if (array(mid) <= t) then
                left = mid
            else
                right = mid
            end if
        end do
    end function


    ! Серия подпрограмм swap. Переставляет что угодно
    ! Пример использования: call swap(matrix(j,:), matrix(k,:))

    elemental subroutine swap_int(a, b)
        integer, intent(inout) :: a, b ! вызывает предупреждения
        integer :: tmp
        tmp = a
        a = b
        b = tmp
    end subroutine

    elemental subroutine swap_real(a, b)
        real(mp), intent(inout) :: a, b ! вызывает предупреждения
        real(mp) :: tmp
        tmp = a
        a = b
        b = tmp
    end subroutine


    ! Серия функций meshgrid. Создаёт матрицу из попарных перемножений
    ! Пример использования: meshgrid([1, 2, 3], [1, 2, 3, 4])

    function meshgrid_int(x, y) result(grid)
        integer, intent(in), dimension(:) :: x, y
        integer, dimension(size(x), size(y)) :: grid
        do concurrent (i=1:size(x), j=1:size(y))
            grid(i,j) = x(i) * y(j)
        end do
    end function

    function meshgrid_real(x, y) result(grid)
        real(mp), intent(in), dimension(:) :: x, y
        real(mp), dimension(size(x), size(y)) :: grid
        do concurrent (i=1:size(x), j=1:size(y))
            grid(i,j) = x(i) * y(j)
        end do
    end function

    
    ! Серия функций multiply. Принимает одномерные и двумерные вещественные массивы
    ! Пример использования: sqrt(sum(multiply(a, x) - b)**2) ! невязка

    pure function multiply_1D_1D(a, b) result(c)
        real(mp), intent(in) :: a(:), b(:)
        real(mp) :: c
        c = dot_product(a, b)
    end function

    pure function multiply_1D_2D(a, b) result(c)
        real(mp), intent(in) :: a(:), b(:,:)
        real(mp) :: c(size(a))
        c = matmul(a, transpose(b))
    end function

    pure function multiply_2D_1D(a, b, mode) result(c)
        real(mp), intent(in) :: a(:,:), b(:)
        real(mp) :: c(size(b))
        character(*), intent(in), optional :: mode
        if (.not. present(mode)) then
            c = matmul(transpose(a), b)
        elseif (mode == 'tridiagonal') then
            block
                real(mp) :: bb(0:size(b)+1)
                bb = 0
                bb(1:size(b)) = b ! расширение в обе стороны нулями
                do concurrent (j=1:size(b))
                    c(j) = dot_product(a(:,j), bb(j-1:j+1))
                end do
            end block
        end if
    end function

    pure function multiply_2D_2D(a, b, mode) result(c)
        real(mp), intent(in) :: a(:,:), b(:,:)
        real(mp), allocatable :: c(:,:)
        character(*), intent(in), optional :: mode ! TODO: проверить на корректность
        integer :: m, j
        m = size(a, dim=2)
        if (.not. present(mode)) then
            c = transpose(matmul(b, a))
        elseif (mode == 'tridiagonal') then ! TODO: select case (mode)
            allocate(c(5, m))
            c = 0
            c(3, 1) = a(2, 1) * b(2, 1) + a(3, 1) * b(1, 2)
            c(4, 1) = a(2, 1) * b(3, 1) + a(3, 1) * b(2, 2)
            c(5, 1) = a(3, 1) * b(3, 2)
            do j=2,m-1
                c(1, j) = a(1, j) * b(1, j-1)
                c(2, j) = a(1, j) * b(2, j-1) + a(2, j) * b(1, j)
                c(3, j) = a(1, j) * b(3, j-1) + a(2, j) * b(2, j) + a(3, j) * b(1, j+1)
                c(4, j) =                       a(2, j) * b(3, j) + a(3, j) * b(2, j+1)
                c(5, j) =                                           a(3, j) * b(3, j+1)
            end do
            c(1, m) = a(1, m) * b(1, m-1)
            c(2, m) = a(1, m) * b(2, m-1) + a(2, m) * b(1, m)
            c(3, m) = a(1, m) * b(3, m-1) + a(2, m) * b(2, m)
        elseif (mode == 'pentadiagonal') then ! работает некорректно
            allocate(c(-4:4, 1:m))
            c = 0
            block
                real(mp) :: aa(-10:10, 1:m), bb(-10:10, -5:m+6)
                aa = 0
                aa(-2:2, 1:m) = a
                bb = 0
                bb(-2:2, 1:m) = b
                do concurrent (i=-4:4, j=1:m)
                    c(i,j) = dot_product(aa(i-6:i+6, j), [(bb(i+6-k, j-6+k), k=0,12)])
                end do
            end block
        end if
    end function

end module