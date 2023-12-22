
! Модуль численного вычисления интегралов

module math_quad
    use io_precision, only: mp
    use io_general, only: isfile
    use io_strings, only: zfill
    use math_general, only: apply
    use math_sle, only: solve_sle
    use math_polynomial, only: solve_polynomial
    implicit none

    private
    public integrate
    
    integer, private :: i, k

    contains

    ! Серия функций integrate. Принимает функцию, интервал и число промежутков
    ! Режим определяется первой буквой из вариантов: rectangle, simpson, gauss
    ! По умолчанию - формула Симпсона (simpson)
    ! Пример использования: integrate(f, 0, 3, 10, 'gauss')

    function integrate(f, a, b, n, mode0) result(s)
        real(mp), intent(in) :: a, b ! пределы интегрирования
        integer, intent(in) :: n ! количество улов
        real(mp) :: s
        character(*), optional :: mode0
        character(1) :: mode
        interface
            pure function f(x) result(y)
                use io_precision, only: mp
                real(mp), intent(in) :: x
                real(mp) :: y
            end function
        end interface
        if (.not. present(mode0)) then
            mode = 's' ! simpson
        else
            mode = mode0
        end if
        select case (mode)
            case ('r') ! rectangle
                s = integrate_rectangle(f, a, b, n-1) ! передаём количество промежутков
            case ('s') ! simpson
                s = integrate_simpson(f, a, b, ceiling(real(n)/2)) ! нужно чётное количество промежутков
            case ('g') ! gauss
                s = integrate_gauss(f, a, b, n) ! передаём количество узлов
        end select
    end function

    function integrate_rectangle(f, a, b, n) result(s)
        real(mp), intent(in) :: a, b
        integer, intent(in) :: n ! количество промежутков
        real(mp) :: s, h, f_array(n+1)
        interface
            pure function f(x) result(y)
                use io_precision, only: mp
                real(mp), intent(in) :: x
                real(mp) :: y
            end function
        end interface
        h = (b - a) / n
        do concurrent (i=1:n)
            f_array(i) = f(a+(i-0.5_mp)*h)
        end do
        s = sum(f_array) * h
    end function

    function integrate_simpson(f, a, b, n) result(s)
        real(mp), intent(in) :: a, b
        integer, intent(in) :: n ! половина количества промежутков
        real(mp) :: s, s1(n), s2(n), h, x
        interface
            pure function f(x) result(y)
                use io_precision, only: mp
                real(mp), intent(in) :: x
                real(mp) :: y
            end function
        end interface
        h = (b - a) / n / 2
        do concurrent (i=1:n)
            x = a+(i*2-1)*h
            s1(i) = f(x)
            s2(i) = f(x+h)
        end do
        s = h/3 * (f(a) - f(b) + 4*sum(s1) + 2*sum(s2))
    end function

    ! Задание 8: метод численного интегрирования Гаусса
    function integrate_gauss(f, a, b, n) result(s)
        real(mp), intent(in) :: a, b
        integer, intent(in) :: n ! количество узлов = степень полинома
        real(mp) :: s, c(n), t(n), scale
        character(10) :: filename
        interface
            pure function f(x) result(y)
                use io_precision, only: mp
                real(mp), intent(in) :: x
                real(mp) :: y
            end function
        end interface
        filename = 'quad'//zfill(n, 2)//'.dat'
        ! Читаем ранее вычисленные коэффициенты из файла, если он есть
        if (isfile(filename)) then
            open(1, file=filename, status='old')
                do i=1,n
                    read(1,*) c(i), t(i)
                end do
            close(1)
        else
            t = solve_polynomial(legendre_polynomial_coefficients(n))
            c = gaussian_quadrature_coefficients(t)
            ! Сохраняем коэффициенты и корни
            open(1, file=filename)
                do i=1,n
                    write(1,*) c(i), t(i)
                end do
            close(1)
        end if
        scale = (b-a) / 2 ! коэффициент масштабирования
        t = a + (t+1) * scale ! масштабирование от [-1, 1] к [a, b]
        s = dot_product(c, apply(f, t)) * scale
    end function

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

end module