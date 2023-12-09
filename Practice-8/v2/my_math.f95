module my_math
    use my_io
    !use, intrinsic :: ieee_arithmetic
    implicit none

    ! Вспомогательные функции
    private spline_vectors, recursive_FFT, legendre_polynomial_coefficients, &
    !private spline_vectors, recursive_FFT, legendre_polynomial_roots, legendre_polynomial_coefficients, &
            solve_polynomial_directly, get_abs_max_root, polynomial_division, isdiagdominant, &
            square_jacobi_matrix, &
            multiply_1D_1D, multiply_1D_2D, multiply_2D_1D, multiply_2D_2D, meshgrid_int, meshgrid_real
    
    interface multiply
        module procedure multiply_1D_1D, multiply_1D_2D, multiply_2D_1D, multiply_2D_2D
    end interface
    
    interface meshgrid
        module procedure meshgrid_int, meshgrid_real
    end interface

    integer, private :: i, j, k
    real(mp), parameter, private :: eps = epsilon(0._mp) ! output accuracy
    real(mp), parameter, private :: sqrt_eps = sqrt(eps) ! differentiation step

    contains


    ! Задание 2: интерполирование полиномами
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

    ! Задание 3: решение СЛУ методом Гаусса
    ! Режим определяется первой буквой из вариантов: gauss, jordan, mainch
    ! По умолчанию - выбор ведущего элемента (mainch)
    function solve_sle(a0, b0, mode0) result(x)
        real(mp), intent(in) :: a0(:,:), b0(:)
        real(mp) :: a(size(b0)+1,size(b0)), x(size(b0))
        integer :: n, lead
        character(*), optional :: mode0
        character(1) :: mode
        if (.not. present(mode0)) then
            mode = 'm' ! (mainch)
        else
            mode = mode0(1)
        end if
        n = size(b0) ! размер СЛУ
        a(:n,:) = a0
        a(n+1,:) = b0
        do k = 1,n ! переход к треугольной матрице
            if (mode == 'm') then ! выбор ведущего элемента (mainch)
                lead =  maxloc(abs(a(k,k:) / maxval(a(:,k:), dim=1)), dim=1) + k-1
                if (lead /= k) then
                    call swap(a(:,k), a(:,lead))
                    !write(*,*) 'Строка '//str(lead)//' теперь ведущая'
                end if
            end if
            !if (abs(a(k,k)) < eps) write(*,*) 'Диагональный элемент итерации '//str(k)//' близок к нулю'
            a(k:,k) = a(k:,k) / a(k,k)
            do concurrent (j=k+1:n)
                a(k:,j) = a(k:,j) - a(k:,k) * a(k,j)
            end do
        end do
        if (mode == 'j') then ! (jordan)
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

    ! Задание 4: решение СЛУ с диагональным преобладанием итерационными методами
    ! Режим определяется первой буквой из вариантов: jacobi, seidel, relax
    ! По умолчанию - метод релаксации (relax)
    function solve_diagdominant_sle(a, b, mode0) result(x)
        real(mp), intent(in) :: a(:,:), b(:)
        real(mp) :: x(size(b))
        integer :: n
        character(*), optional :: mode0
        character(1) :: mode
        if (.not. present(mode0)) then
            mode = 'r' ! (relax)
        else
            mode = mode0(1)
        end if
        if (.not. isdiagdominant(a)) stop 'Матрица не имеет диагонального преобладания!'
        n = size(b) ! размер СЛУ
        select case (mode)
            case ('j') ! метод Якоби
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
            case ('s') ! метод Зейделя
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
            case ('r') ! метод релаксации
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
        end select
    end function

    ! Задание 5: решение СЛУ методом пятиточечной прогонки
    ! Принимает пятидиагональную матрицу a0 в компактном виде, из пяти колонок
    function solve_pentadiagdominant_sle(a0, b0) result(x)
        real(mp), intent(in) :: a0(:,:), b0(:)
        real(mp) :: x(size(b0)), a(-1:size(b0)), b(-1:size(b0)), c(-1:size(b0)), &
                    alpha, beta, p(-1:size(b0)), q(-1:size(b0)), r(-1:size(b0))
        integer :: n
        n = size(b0) ! размер СЛУ
        a = 0
        b = 0
        c = 0
        a(1:) = a0(3,:) ! переобозначение колонок по условию задания
        b(1:) = a0(4,:) !     и расширение первых двух элементов нулями
        c(1:) = a0(5,:) !     для обращений по i-2 и i-1
        p = 0
        q = 0
        r = 0
        ! Прямой ход
        do i = 1,n
            beta = b(i-1) - p(i-2) * c(i-2)
            alpha = a(i) - p(i-1) * beta - q(i-2) * c(i-2)
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

    ! Задание 5: аппроксимация сплайнами
    function spline_approx(XYP, q) result(approximated)
        integer, intent(in) :: q ! число разбиений интервала
        integer :: n, m
        real(mp), intent(in) :: XYP(:, 0:) ! начальная сетка в виде колонок X, Y, P
        real(mp) :: approximated(2, 0:(size(XYP, dim=2)-1)*q), & ! результат в виде X, Y
                    s(0:size(XYP, dim=2)-1), r(0:size(XYP, dim=2)-1), & ! промежуточные вектора
                    h(0:size(XYP, dim=2)-2), h_j, xi, t
        n = size(XYP, dim=2) - 1 ! число интервалов входной сетки
        call spline_vectors(XYP, n, s, r)
        m = n * q ! число интервалов новой сетки
        h_j = (XYP(1,n) - XYP(1,0)) / m ! шаг новой сетки
        h = diffs(XYP(1,:)) ! предвычисленный массив разностей иксов
        do concurrent (j=0:m) ! можно было итерировать по входной сетке, но так должно быть быстрее
            xi = XYP(1,0) + h_j * j ! текущая точка
            i = find_index(XYP(1,:), xi) - 1 ! сдвиг из-за местного отсчёта от нуля
            t = (xi - XYP(1,i)) / h(i)
            approximated(1,j) = xi
            approximated(2,j) = r(i)*(1-t) + r(i+1)*t - h(i)*h(i)*t*(1-t)*((2-t)*s(i)+(1+t)*s(i+1))/6
        end do
    end function

    ! Вспомогательная функция, формирующая вектора R и S
    subroutine spline_vectors(XYP, n, s, r)
        integer, intent(in) :: n ! передаю внутрь для упрощения инициализации размеров ниже
        real(mp), intent(in) :: XYP(1:3, 0:n)
        real(mp), intent(inout) :: s(n+1), r(n+1)
        real(mp) :: a(-1:1, n+1), b(-1:1, n+1), qbt(-1:1, n+1), aa(-2:2, n+1) ! трёхдиагональные и пятидиагональные
        a = 0
        a(0,1) = 2*XYP(1,1) - 2*XYP(1,0) ! напоминание, что здесь "XYP(1,:)" - это колонка иксов
        a(0,2) = 2*XYP(1,2) - 2*XYP(1,0) ! 2 за скобку не выношу, т.к. "вычитать почти равные числа - плохо"
        a(1,2) = XYP(1,2) - XYP(1,1)
        a(-1,n) = XYP(1,n-1) - XYP(1,n-2)
        a(0,n) = 2*XYP(1,n) - 2*XYP(1,n-2)
        a(0,n+1) = 2*XYP(1,n) - 2*XYP(1,n-1)
        b = 0
        b(-1,2) = 1/(XYP(1,1) - XYP(1,0))  ! предрасчитываю вручную первые и последние две строки, а не расширяю нулями, т.к.
        b(1,2) = 1/a(1,2)                  ! 1) эти матрицы потом перемножаются, менять их размеры не удобно
        b(0,2) = -b(-1,2) - b(1,2)         ! 2) для матрицы A другого варианта нет
        b(-1,n) = 1/a(-1,n)
        b(1,n) = 1/(XYP(1,n) - XYP(1,n-1))
        b(0,n) = -b(-1,n) - b(1,n)
        do concurrent (i=3:n-1) ! быстрее за счёт распараллеливания, но больше вычислений, чем если итеративно
            a(-1,i) = XYP(1,i-1) - XYP(1,i-2)
            a(1, i) = XYP(1,i) - XYP(1,i-1)
            a(0, i) = 2*XYP(1,i) - 2*XYP(1,i-2)
            b(-1,i) = 1/a(-1,i)
            b(1, i) = 1/a(1,i)
            b(0, i) = -b(-1,i) - b(1,i)
        end do
        qbt = 0
        do concurrent (i=2:n) ! одновременное транспонирование B и умножение на Q: тогда будет 3-диг. матрица, а не 5-диг.
            qbt(-1,i+1) = b(-1,i) / XYP(3,i)
            qbt(0,i) = b(0,i) / XYP(3,i-1)
            qbt(1,i-1) = b(1,i) / XYP(3,i-2)
        end do
        aa = 6 * multiply(b, qbt, 'tridiagonal')
        aa(-1:1,:) = aa(-1:1,:) + a ! правая часть СЛУ
        !call output('aa = ', aa) ! слегка несимметрична?
        s = solve_pentadiagdominant_sle(aa, 6 * multiply(b, XYP(2,:), 'tridiagonal'))
        r = XYP(2,:) - multiply(qbt, s, 'tridiagonal') ! вектор результатов R = Y - Q B^T S
    end subroutine

    ! Задание 6: многомерный метод Ньютона
    function newton(f, initial_vector, iterations_limit) result(x1)
        real(mp), intent(in) :: initial_vector(:)
        integer, intent(in), optional :: iterations_limit
        real(mp), dimension(size(initial_vector)) :: x0, x1
        real(mp), dimension(size(initial_vector), size(initial_vector)) :: jacobi_matrix
        integer :: limit
        interface
            pure function f(x) result(y)
                use my_io, only: mp
                real(mp), intent(in), dimension(:) :: x
                real(mp), dimension(size(x)) :: y
            end function
        end interface
        if (.not. present(iterations_limit)) then
            limit = 100
        else
            limit = iterations_limit
        end if
        x0 = initial_vector
        do i = 1,limit
            !call output('iteration '//str(i)//': x0 =', x0)
            jacobi_matrix = square_jacobi_matrix(f, x0)
            x1 = solve_sle(jacobi_matrix, multiply(jacobi_matrix, x0) - f(x0))
            if (all(abs(x1 - x0) < eps)) exit
            x0 = x1
        end do
    end function

    ! Вычисление квадратной матрицы Якоби
    function square_jacobi_matrix(f, x) result(jacobi_matrix)
        real(mp), intent(in) :: x(:)
        real(mp), dimension(size(x)) :: f1, f2, step
        real(mp), dimension(size(x), size(x)) :: jacobi_matrix
        integer :: n
        interface
            pure function f(x) result(y)
                use my_io, only: mp
                real(mp), intent(in), dimension(:) :: x
                real(mp), dimension(size(x)) :: y
            end function
        end interface
        n = size(x)
        do concurrent (i=1:n)
            step = kronecker_delta(i, n) * sqrt_eps
            f1 = f(x - step)
            f2 = f(x + step)
            jacobi_matrix(i,:) = (f2-f1) / (2*sqrt_eps)
        end do
    end function

    ! Символ Кронекера. Возвращает нулевой n-вектор с единицей на i-том месте.
    pure function kronecker_delta(index, n) result(array)
        integer, intent(in) :: index, n
        real(mp), dimension(n) :: array
        array(:) = 0
        array(index) = 1
    end function

    ! Задание 7: Быстрое преобразование Фурье
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

    ! Задание 8: метод численного интегрирования Гаусса

    ! Вычисляет коэффициенты квадратурной формулы Гаусса
    function gaussian_quadrature_coefficients(roots) result(a_vector)
        real(mp), intent(in) :: roots(:)
        real(mp) :: t_matrix(0:size(roots)-1,0:size(roots)-1), &
                    b_vector(size(roots)), a_vector(size(roots))
        integer :: n
        n = size(roots)
        ! рекуррентно конструируем матрицу коэффициентов СЛУ
        t_matrix(:,0) = 1
        t_matrix(:,1) = roots
        do k=2,n-1
            t_matrix(:,k) = t_matrix(:,k-1) * roots
        end do
        ! конструируем вектор результатов СЛУ
        b_vector = 0
        do concurrent (k=1:n:2)
            b_vector(k) = 2. / k
        end do
        ! ищем коэффициенты A через СЛУ
        a_vector = solve_sle(t_matrix, b_vector)
    end function

    ! Вычисляет корни полинома Лежандра
    function legendre_polynomial_roots(n) result(roots)
        integer, intent(in) :: n
        real(mp), allocatable :: roots(:)
        roots = solve_polynomial(legendre_polynomial_coefficients(n))
    end function

    ! Вычисляет коэффициенты полинома Лежандра
    function legendre_polynomial_coefficients(n) result(coeffs)
        integer, intent(in) :: n
        real(mp) :: coeffs0(0:n), coeffs1(0:n), coeffs(0:n)
        select case (n)
            case (0)
                coeffs = 1
            case (1)
                coeffs = [1, 0]
            case (2:)
                coeffs0 = 0
                coeffs1 = 0
                coeffs0(n) = 1 ! P_0(x) = 1
                coeffs1(n-1) = 1 ! P_1(x) = x
                do i = 2,n
                    coeffs(:n-1) = coeffs1(1:) ! эффект умножения на x
                    coeffs(n) = 0
                    coeffs = ((2*i-1) * coeffs - (i-1) * coeffs0) / i
                    coeffs0 = coeffs1
                    coeffs1 = coeffs
                end do
        end select
    end function

    ! Находит вещественные корни полинома `P(x) = a_0 x^n + ... + a_n = 0`
    function solve_polynomial(a) result(x)
        real(mp), intent(in) :: a(0:)
        real(mp) :: x(size(a)-1)
        integer :: n
        n = size(a) - 1 ! фактическая степень многочлена, стремимся уменьшить
        x = 0
        ! делим на столько иксов, на сколько можем
        do i = n,1,-1
            if (a(i) /= 0) exit
            n = n - 1
        end do
        ! проверяем делимость на x^2
        if (all(a(1:n:2) == 0)) then
            x(1:n:2) = sqrt(solve_polynomial_directly(a(0:n:2)))
            x(2:n:2) = -x(1:n:2)
        else
            x(:n) = solve_polynomial_directly(a(:n))
        end if
    end function

    ! Находит вещественные корни полинома напрямую, без попытки упростить
    function solve_polynomial_directly(a) result(x)
        real(mp), intent(in) :: a(0:)
        real(mp) :: x(size(a)-1), a1(0:size(a)-1)
        integer :: n, m
        n = size(a) - 1 ! степень многочлена
        select case (n)
            case (1)
                x = - a(1) / a(0) ! массив из одного элемента
            case (2)
                x = solve_quadratic_equation(a(0), a(1), a(2))
            case (3:)
                a1 = a
                do m = n,3,-1
                    x(m) = get_abs_max_root(a1(:m))
                    a1 = polynomial_division(a1(:m), x(m))
                end do
                x(1:2) = solve_quadratic_equation(a1(0), a1(1), a1(2))
        end select
    end function

    ! Решает квадратное уравнение в вещественных числах
    function solve_quadratic_equation(a, b, c) result(x)
        real(mp), intent(in) :: a, b, c
        real(mp) :: a2, d, x(2), left_part, right_part
        a2 = 1 / (2*a)
        left_part = -b * a2
        d = b*b - 4*a*c
        if (d < 0) then
            ! Эксперименты показали, что когда дискриминант близок к нулю,
            ! погрешность метода Бернулли иногда делает корни комплексными,
            ! и возвращается [NaN, NaN].
            ! Поэтому беру ближайший вещественный сдвоенный корень.
            x = left_part
        else
            right_part = sqrt(d) * a2
            x(1) = left_part - right_part
            x(2) = left_part + right_part
        end if
    end function

    ! Поиск максимального по модулю корня полинома методом Бернулли
    function get_abs_max_root(a, iterations_limit) result(x1)
        real(mp), intent(in) :: a(0:)
        integer, intent(in), optional :: iterations_limit
        real(mp) :: x1, y(1:size(a)-1), b(1:size(a)-1)
        integer :: n, limit
        if (.not. present(iterations_limit)) then
            limit = huge(1)
        else
            limit = iterations_limit
        end if
        n = size(a) - 1 ! степень многочлена
        b = a(1:) / a(0) ! приведённые коэффициенты
        y = 1 ! n начальных значений
        do i=1,limit
            x1 = - dot_product(y, b) ! x1 тут - буферная переменная
            y(2:n) = y(1:n-1) ! сдвиг: новый элемент в начало
            y(1) = x1
            do while (abs(y(1)) < 0.03125)
                y = y * 1024
            end do
            x1 = y(1)/y(2)
            if (abs(x1 - y(2)/y(3)) < eps) exit
        end do
        !write(*,*) 'Для поиска корня '//str(x1)//' потребовалось '//str(i-1)//' итераций'
    end function

    ! Делит `P(x) = a_0 x^n + ... + a_n` на `x - x0` методом Горнера
    function polynomial_division(a, x0) result(b)
        real(mp), intent(in) :: a(0:), x0
        real(mp) :: b(0:size(a)-2)
        integer :: n
        n = size(a) - 1 ! степень многочлена
        b(0) = a(0)
        do i = 1,n-1
            b(i) = b(i-1) * x0 + a(i)
        end do
        !r = b(n-1) * x0 + a(n) ! остаток
    end function

    ! Задание 9: системы обыкновенных дифференциальных уравнений

    ! Интуитивное решение ДУ для тестирования
    function ode_simple(t, x0) result(x)
        real(mp), intent(in) :: t(:), x0(:)
        real(mp) :: x(size(x0), size(t))
        call random_number(x)
    end function

    ! Метод Рунге-Кутты 4-го порядка
    function ode_runge_kutta_4(t, x0) result(x)
        real(mp), intent(in) :: t(:), x0(:)
        real(mp) :: x(size(x0), size(t))
        call random_number(x)
    end function

    ! Экстраполяционный метод Адамса
    function ode_adams_extrap(t, x0) result(x)
        real(mp), intent(in) :: t(:), x0(:)
        real(mp) :: x(size(x0), size(t))
        call random_number(x)
    end function

    ! Интерполяционный метод Адамса
    function ode_adams_interp(t, x0) result(x)
        real(mp), intent(in) :: t(:), x0(:)
        real(mp) :: x(size(x0), size(t))
        call random_number(x)
    end function


    ! Серия функций integrate. Принимает функцию, интервал и число промежутков
    ! Пример использования: integrate(f, 0, 10, 100, 'rectangle')

    function integrate(f, a, b, n, mode) result(s)
        integer :: n, m
        real(mp), external :: f
        real(mp) :: s, a, b, h
        character(*), optional :: mode
        if (.not. present(mode)) then
            mode = 'simpson'
        end if
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
        elseif (mode == 'simpson') then
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
        character(*), intent(in), optional :: mode
        integer :: m, j
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

    ! Возвращает .true., если у матрицы имеется диагональное преобладание
    pure logical function isdiagdominant(a) 
        real(mp), intent(in) :: a(:,:)
        isdiagdominant = all([( ( 2*abs(a(j,j)) >= sum(abs(a(:,j))) ), j=1,size(a, dim=2) )])
    end function

    ! Сжимает пятидиагональную матрицу (не используется)
    function compressed(matrix)
        real(mp), intent(in) :: matrix(:,:)
        real(mp) :: compressed(5, size(matrix, dim=2))
        integer :: n
        n = size(matrix, dim=2)
        compressed = 0
        compressed(1, 3:n) = [( matrix(i,i+2), i=1,n-2 )]
        compressed(2, 2:n) = [( matrix(i,i+1), i=1,n-1 )]
        compressed(3, 1:n) = [( matrix(i,i),   i=1,n   )]
        compressed(4, 1:n-1) = [( matrix(i,i-1), i=2,n )]
        compressed(5, 1:n-2) = [( matrix(i,i-2), i=3,n )]
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

    ! Создание массива промежутков между элементами
    pure function diffs(array)
        real(mp), intent(in) :: array(:)
        real(mp) :: diffs(size(array)-1)
        diffs = array(2:size(array)) - array(1:size(array)-1)
    end function

    ! Серия функций meshgrid: создаёт матрицу из попарных перемножений

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

end module