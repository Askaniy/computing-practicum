module my_math
    use my_io !use, intrinsic :: ieee_arithmetic
    implicit none

    private
    public dist, solve_sle, solve_diagdominant_sle, solve_pentadiagdominant_sle, polynomial_interp, integrate, multiply, isdiagdominant
    
    interface multiply
        module procedure multiply_1D_1D, multiply_1D_2D, multiply_2D_1D, multiply_2D_2D
    end interface

    integer :: i, j, k
    real, parameter, private :: eps = 10.0_mp**(-dp)

    contains


    pure function dist(a, b) ! возвращает евклидову метрику
        real(mp), intent(in) :: a(:), b(:)
        real(mp) :: dist
        dist = sqrt(sum(a - b)**2)
    end function

    pure logical function isdiagdominant(a) ! возвращает .true., если у матрицы имеется диагональное преобладание
        real(mp), intent(in) :: a(:,:)
        isdiagdominant = all([( ( 2*abs(a(j,j)) >= sum(abs(a(:,j))) ), j=1,size(a, dim=2) )])
    end function

    function solve_sle(a0, b0, mode) result(x)
        real(mp), intent(in) :: a0(:,:), b0(:)
        real(mp) :: a(size(b0)+1,size(b0)), x(size(b0))
        integer :: n, lead
        character(*), optional :: mode
        write(*,*) 'Решение системы в режиме "'//mode//'"'
        n = size(b0) ! размер СЛУ
        a(:n,:) = a0
        a(n+1,:) = b0
        !call output('k=0, a =', a)
        do k = 1,n ! переход к треугольной матрице
            if (mode == 'mainch') then ! выбор ведущего элемента
                lead =  maxloc(abs(a(k,k:) / maxval(a(:,k:), dim=1)), dim=1) + k-1
                if (lead /= k) then
                    call swap(a(:,k), a(:,lead))
                    write(*,*) 'Строка '//str(lead)//' теперь ведущая'
                end if
            end if
            if (abs(a(k,k)) < eps) write(*,*) 'Диагональный элемент итерации '//str(k)//' близок к нулю'
            a(k:,k) = a(k:,k) / a(k,k)
            do concurrent (j=k+1:n)
                a(k:,j) = a(k:,j) - a(k:,k) * a(k,j)
            end do
            !call output('k='//str(k)//', a =', a)
        end do
        if (mode == 'jordan') then
            do concurrent (j=n:1:-1)
                a(n+1,1:j-1) = a(n+1,1:j-1) - a(j,1:j-1) * a(n+1,j)
            end do
            x = a(n+1, :)
        else ! схема с Гауссом или выбором
            x(n) = a(n+1,n)
            do concurrent (j=n-1:1:-1)
                x(j) = a(n+1,j) - dot_product(a(j+1:n,j), x(j+1:n))
            end do
        end if
    end function

    function solve_diagdominant_sle(a, b, mode) result(x)
        real(mp), intent(in) :: a(:,:), b(:)
        real(mp) :: x(size(b))
        integer :: n
        character(*), optional :: mode
        write(*,*) 'Решение системы в режиме "'//mode//'"'
        if (.not. isdiagdominant(a)) stop 'Матрица не имеет диагонального преобладания!'
        n = size(b) ! размер СЛУ
        if (mode == 'jacobi') then ! Итерационный метод Якоби решения СЛУ
            block
                real(mp) :: x0(n), g(n), d_z(n,n), d_rev(n,n)
                x0 = b ! произвольно
                d_z = 0 ! матрица для хранения D и Z, сначала D (диагональ A)
                d_rev = 0 ! матрица для хранения D^(-1)
                do concurrent (j=1:n)
                    d_z(j,j) = a(j,j)
                    d_rev(j,j) = 1.0 / a(j,j)
                end do
                d_z = multiply(d_rev, d_z-a) ! теперь Z
                g = multiply(d_rev, b)
                do
                    x = matmul(d_z, x0) + g ! почему-то multiply не работает
                    if (dist(x0, x) < eps) exit
                    x0 = x
                end do
            end block
        else if (mode == 'seidel') then ! Итерационный метод Зейделя решения СЛУ
            block
                real(mp) :: x0(n), p_j(n)
                x0 = b ! произвольно
                do
                    do j=1,n
                        p_j = -a(:,j) / a(j,j)
                        x(j) = dot_product(p_j(1:j-1), x(1:j-1)) + dot_product(p_j(j+1:n), x0(j+1:n)) + b(j)/a(j,j)
                    end do
                    if (dist(x0, x) < eps) exit
                    x0 = x
                end do
            end block
        else if (mode == 'relax') then ! Итерационный метод релаксации решения СЛУ
            block
                real(mp) :: p(n,n), q(n)
                integer :: max_index
                do j=1,n
                    p(:,j) = -a(:,j) / a(j,j)
                    q(j) = b(j) / a(j,j) ! вектор невязок
                end do
                x = 0 ! обязательно
                do
                    max_index = maxloc(abs(q), dim=1)
                    x(max_index) = x(max_index) + q(max_index)
                    if (abs(q(max_index)) < eps) exit
                    q = q + p(max_index,:) * q(max_index)
                end do
            end block
        end if
    end function

    function solve_pentadiagdominant_sle(a0, b0) result(x)
        real(mp), intent(in) :: a0(:,:), b0(:)
        real(mp) :: x(size(b0)), a(-1:size(b0)), b(-1:size(b0)), c(-1:size(b0)), &
                    alpha, beta, p(-1:size(b0)), q(-1:size(b0)), r(-1:size(b0))
        integer :: n
        n = size(b0) ! размер пятидиагональной симметричной СЛУ с диагональным преобладанием
        a = 0
        b = 0
        c = 0
        a(1:) = a0(1,:) ! переобозначение колонок по условию
        b(1:) = a0(2,:) !     и расширение первых двух элементов нулями
        c(1:) = a0(3,:) !     для обращений по i-2 и i-1
        p = 0
        q = 0
        r = 0
        ! Прямой ход
        do i = 1,n
            beta = b(i-1) - p(i-2) * c(i-2)
            alpha = a(i-1) - p(i-1) * beta - q(i-2) * c(i-2)
            p(i) = (b(i) - q(i-1) * beta) / alpha
            q(i) = c(i) / alpha
            r(i) = (b0(i) - r(i-1) * beta - r(i-2) * c(i-2)) / alpha
        end do
        ! Обратный ход
        x(n) = r(n)
        x(n-1) = r(n-1) - p(n-1) * x(n)
        do i = n-2,1,-1
            x(i) = r(i) - p(i) * x(i+1) - q(i) * x(i+2)
        end do
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

    function multiply_1D_1D(a, b) result(c)
        real(mp), intent(in) :: a(:), b(:)
        real(mp) :: c
        c = dot_product(a, b)
    end function

    function multiply_1D_2D(a, b) result(c)
        real(mp), intent(in) :: a(:), b(:,:)
        real(mp) :: c(size(a))
        c = matmul(a, transpose(b))
    end function

    function multiply_2D_1D(a, b) result(c)
        real(mp), intent(in) :: a(:,:), b(:)
        real(mp) :: c(size(b))
        c = matmul(transpose(a), b)
    end function

    function multiply_2D_2D(a, b, mode) result(c)
        real(mp), intent(in) :: a(:,:), b(:,:)
        real(mp), allocatable :: c(:,:)
        character(*), optional :: mode
        integer :: m
        m = size(a, dim=2)
        if (.not. present(mode)) then
            c = transpose(matmul(b, a))
        elseif (mode == 'tridiagonal') then
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
        elseif (mode == 'pentadiagonal') then
            allocate(c(-4:4, 1:m))
            c = 0
            block
                real(mp) :: aa(-10:10, 1:m), bb(-10:10, -5:m+6)
                aa = 0
                aa(-2:2, 1:m) = a
                bb = 0
                bb(-2:2, 1:m) = b
                do concurrent (i=-4:4, j=1:m)
                    c(i, j) = dot_product(aa(i-6:i+6, j), [(bb(i+6-k, j-6+k), k=0,12)])
                end do
            end block
        end if
    end function

end module