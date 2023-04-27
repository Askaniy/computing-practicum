module my_math
    use my_io !use, intrinsic :: ieee_arithmetic
    implicit none

    private
    public solve_sle, polynomial_interp, integrate, multiply
    
    interface multiply
        module procedure multiply_1Dvar0, multiply_1Dvar1, multiply_1Dvar2, multiply_2D
    end interface

    integer, private :: i, j, k

    contains

    function solve_sle(a0, b0, mode) result(x)
        real(mp), intent(in) :: a0(:,:), b0(:)
        real(mp) :: a(size(b0)+1,size(b0)), x(size(b0))!, temp_row(size(b0)+1)
        integer :: n, loc
        character(*), optional :: mode
        write(*,*) 'Решение системы в режиме "'//mode//'"'
        n = size(b0) ! размер СЛУ
        a(:n,:) = a0
        a(n+1,:) = b0
        call output('k=0, a =', a)
        do k = 1,n ! Переход к треугольной матрице
            if (mode == 'mainch') then
                !call output('Ищем максимум среди ', a(:,k:))
                loc = maxloc(abs( a(k,k:) / maxval(a(:,k:), dim=1) ), dim=1)
                !write(*,*) loc
            end if
            if (abs(a(k,k)) < 0.01) write(*,*) 'Диагональный элемент итерации '//str(k)//' близок к нулю'
            a(k:,k) = a(k:,k) / a(k,k)
            forall (j=k+1:n) a(k:,j) = a(k:,j) - a(k:,k) * a(k,j)
            call output('k='//str(k)//', a =', a)
        end do
        if (mode == 'jordan') then
            do j=n,1,-1
                a(n+1,1:j-1) = a(n+1,1:j-1) - a(j,1:j-1) * a(n+1,j)
            end do
            x = a(n+1, :)
        else ! схема с Гауссом или выбором
            x(n) = a(n+1,n)
            do j=n-1,1,-1
                x(j) = a(n+1,j) - dot_product(a(j+1:n,j), x(j+1:n))
            end do
        end if
    end function

    function polynomial_interp(grid, q, a, b) result(interpolated)
        integer, intent(in) :: q ! число разбиений интервала
        integer :: n, m
        real(mp), intent(in) :: grid(:, 0:), a, b ! начальная сетка в виде колонок x, f(x)
        real(mp) :: interpolated(2, 0:(size(grid, dim=2)-1)*q) ! результат в том же виде
        n = size(grid, dim=2) - 1
        m = size(interpolated, dim=2) - 1 ! число интервалов
        interpolated(1,:) = [(a + j*(b-a)/m, j=0,m)]
        interpolated(2,:) = [(dot_product( & ! интерполяционный полином
                                grid(2,:), [(product( & ! интерполяционный базис
                                    (interpolated(1,j) - grid(1,:)) / (grid(1,k) - grid(1,:)), &
                                    mask = grid(1,k)/=grid(1,:)), k=0,n)]), &
                            j=0,m)]
    end function


    ! Серия функций integrate. Принимает функцию, интервал и число промежутков
    ! Пример использования: integrate(f, 0, 10, 100, 'rectangle')

    function integrate(f, a, b, n, mode) result(s)
        integer :: n, m
        real(mp), external :: f
        real(mp) :: s, a, b, h
        character(*), optional :: mode
        h = (b - a) / n
        if (mode == 'rectangle') then
            s = 0
            do i = 1,n
                s = s + f(a+(i-0.5_mp)*h)
            end do
            s = s * h
        elseif (mode == 'trapeze') then
            s = (f(a) + f(b)) / 2
            do i = 2,n
                s = s + f(a+(i-1)*h)
            end do
            s = s * h
        elseif (mode == 'simpson' .or. .not. present(mode)) then ! выбор по умолчанию
            m = n
            if (mod(n, 2) == 1) then !s = ieee_value(s, ieee_quiet_nan); return
                m = n + 1
            end if
            s = simpson(f, a, b, m)
        end if
    end function

    function simpson(f, a, b, n) result(s)
        integer :: n
        real(mp) :: s, s1, s2, a, b, h, f, x
        h = (b - a) / n
        s1 = 0
        s2 = -f(b)
        do i = 2,n,2
            x = a+(i-1)*h
            s1 = s1 + f(x)
            s2 = s2 + f(x+h)
        end do
        s = h/3 * (f(a) + f(b) + 4*s1 + 2*s2)
    end function


    ! Серия функций multiply. Принимает одномерные и двумерные вещественные массивы
    ! Пример использования: sqrt(sum(multiply(a, x) - b)**2) ! невязка

    function multiply_1Dvar0(a, b) result(c)
        real(mp), intent(in) :: a(:), b(:)
        real(mp) :: c(size(b))
        c = dot_product(a, b)
    end function

    function multiply_1Dvar1(a, b) result(c)
        real(mp), intent(in) :: a(:,:), b(:)
        real(mp) :: c(size(b))
        c = matmul(transpose(a), b)
    end function

    function multiply_1Dvar2(a, b) result(c)
        real(mp), intent(in) :: a(:), b(:,:)
        real(mp) :: c(size(a))
        c = matmul(a, transpose(b))
    end function

    function multiply_2D(a, b, mode) result(c)
        real(mp), intent(in) :: a(:,:), b(:,:)
        real(mp), allocatable :: c(:,:)
        character(*), optional :: mode
        if (.not. present(mode)) then
            c = transpose(matmul(transpose(a), transpose(b)))
        elseif (mode == 'tridiagonal') then ! не протестировано!
            c = tridiagonal(a, b)
        end if
    end function

    function tridiagonal(a, b) result(c)
        integer :: m
        real(mp), intent(in) :: a(:,:), b(:,:)
        real(mp), allocatable :: c(:,:)
        m = size(a, dim=2)
        allocate(c(5, m))
        c = 0
        c(1:2, 1) = 0 ! NaN
        c(3, 1) = a(2, 1) * b(2, 1) + a(3, 1) * b(1, 2)
        c(4, 1) = a(2, 1) * b(3, 1) + a(3, 1) * b(2, 2)
        c(5, 1) = a(3, 1) * b(3, 2)
        do j=2,m-1
            c(1, j) = a(1, j) * b(1, j-1)
            c(2, j) = a(1, j) * b(2, j-1) + a(2, j) * b(1, j)
            c(3, j) = a(1, j) * b(3, j-1) + a(2, j) * b(2, j) + a(3, j) * b(1, j+1)
            c(4, j) = a(2, j) * b(3, j) + a(3, j) * b(2, j+1)
            c(5, j) = a(3, j) * b(3, j+1)
        end do
        c(1, m) = a(1, m) * b(1, m-1)
        c(2, m) = a(1, m) * b(2, m-1) + a(2, m) * b(1, m)
        c(3, m) = a(1, m) * b(3, m-1) + a(2, m) * b(2, m)
        c(4:5, m) = 0 ! NaN
    end function

end module