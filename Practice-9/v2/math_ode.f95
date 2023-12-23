
! Модуль поиска решения систем обыкновенных дифференциальных уравнений

module math_ode
    use io_precision, only: mp
    use math_general, only: factorial, diffs
    use math_quad, only: integrate
    use math_newton, only: newton
    implicit none

    private
    public ode_simple, ode_runge_kutta_4, ode_adams_extrap, ode_adams_interp
    
    integer, private :: i, j, k

    contains

    ! Метод Эйлера для тестирования
    function ode_simple(f, t, x0) result(x)
        real(mp), intent(in) :: t(:), x0(:)
        real(mp) :: h(2:size(t)), x(size(x0), size(t))
        integer :: n, d
        interface
            pure function f(t, x) result(x_dot)
                use io_precision, only: mp
                real(mp), intent(in) :: t, x(:)
                real(mp) :: x_dot(size(x))
            end function
        end interface
        n = size(t)  ! количество шагов
        d = size(x0) ! размерность системы
        h = diffs(t) ! поддержка переменного шага
        x(:,1) = x0  ! начальные данные
        do i=2,n
            x(:,i) = x(:,i-1) + f(t(i), x(:,i-1)) * h(i)
        end do
    end function

    ! Метод Рунге-Кутты 4-го порядка
    function ode_runge_kutta_4(f, t, x0) result(x)
        real(mp), intent(in) :: t(:), x0(:)
        real(mp) :: h, h2, x(size(x0), size(t)), &
                    k1(size(x0)), k2(size(x0)), k3(size(x0)), k4(size(x0))
        integer :: n, d
        interface
            pure function f(t, x) result(x_dot)
                use io_precision, only: mp
                real(mp), intent(in) :: t, x(:)
                real(mp) :: x_dot(size(x))
            end function
        end interface
        n = size(t)   ! количество шагов
        d = size(x0)  ! размерность системы
        h = t(2)-t(1) ! предполагается равномерная сетка
        h2 = h/2
        x(:,1) = x0   ! начальные данные
        do i=2,n
            k1 = h * f(t(i-1), x(:,i-1))
            k2 = h * f(t(i-1)+h2, x(:,i-1)+k1/2)
            k3 = h * f(t(i-1)+h2, x(:,i-1)+k2/2)
            k4 = h * f(t(i-1), x(:,i-1)+k3)
            x(:,i) = x(:,i-1) + (k1 + 2*k2 + 2*k3 + k4) / 6
        end do
    end function

    ! Экстраполяционный метод Адамса
    function ode_adams_extrap(f, t, x0, m) result(x)
        real(mp), intent(in) :: t(:), x0(:)
        integer, intent(in) :: m ! порядок метода
        real(mp) :: h, x(size(x0), size(t)), y(size(x0), size(t)), a(m)
        integer :: n, d
        interface
            pure function f(t, x) result(x_dot)
                use io_precision, only: mp
                real(mp), intent(in) :: t, x(:)
                real(mp) :: x_dot(size(x))
            end function
        end interface
        n = size(t)   ! количество шагов
        d = size(x0)  ! размерность системы
        h = t(2)-t(1) ! предполагается равномерная сетка
        a = adams_coefficients(m, mode=0)
        x(:,:m) = ode_runge_kutta_4(f, t(:m), x0) ! начальные данные
        do concurrent (i=1:m-1) ! заполнение массива значений функции
            y(:,i) = f(t(i), x(:,i))
        end do
        do i=m+1,n
            y(:,i-1) = f(t(i-1), x(:,i-1))
            x(:,i) = x(:,i-1) + h * matmul(y(:,i-1:i-m:-1), a)
        end do
    end function

    ! Интерполяционный метод Адамса
    function ode_adams_interp(f, t, x0, m) result(x)
        real(mp), intent(in) :: t(:), x0(:)
        integer, intent(in) :: m ! порядок метода
        real(mp) :: h, x(size(x0), size(t)), y(size(x0), size(t)), b(m)
        integer :: n, d
        interface
            pure function f(t, x) result(x_dot)
                use io_precision, only: mp
                real(mp), intent(in) :: t, x(:)
                real(mp) :: x_dot(size(x))
            end function
        end interface
        n = size(t)   ! количество шагов
        d = size(x0)  ! размерность системы
        h = t(2)-t(1) ! предполагается равномерная сетка
        b = adams_coefficients(m, mode=1)
        x(:,:m) = ode_runge_kutta_4(f, t(:m), x0) ! начальные данные
        do concurrent (i=1:m-1) ! заполнение массива значений функции
            y(:,i) = f(t(i), x(:,i))
        end do
        do j=m,n ! переменная i конфликтует с i из функции newton
            x(:,j) = newton(se, x(:,j-1))
            y(:,j) = f(t(j), x(:,j))
        end do

        contains

        pure function se(vector) result(res)
            use io_precision, only: mp
            real(mp), intent(in) :: vector(:)
            real(mp) :: y_temp(d,m), res(d)
            y_temp = y(:,j:j-m+1:-1)
            y_temp(:,1) = f(t(i), vector)
            res = vector - x(:,j-1) - h * matmul(y_temp, b)
        end function
    end function

    ! Вычисление коэффициентов методов Адамса
    ! Экстраполяционный метод: mode = 0
    ! Интерполяционный метод:  mode = 1
    function adams_coefficients(n, mode) result(a)
        integer, intent(in) :: n, mode
        real(mp) :: a(0:n-1), integral
        do concurrent (j=0:n/2)
            a(j) = factorial(j) * factorial(n-1-j)
            a(n-1-j) = a(j)
        end do
        if (mod(n, 2) /= 0) then
            a(n/2) = factorial(n/2)
            a(n/2) = a(n/2) * a(n/2)
        end if
        a(1:n-1:2) = -a(1:n-1:2)
        do j=0,n-1
            call adams_integral(integral, n, j-mode, mode)
            a(j) = integral / a(j)
        end do
    end function

    subroutine adams_integral(integral, n, m, mode)
        integer, intent(in) :: n, m, mode
        real(mp), intent(out) :: integral
        integral = integrate(f, 0._mp, 1._mp, 10, 'gauss')
        contains
        pure function f(z) result(res)
            use io_precision, only: mp
            real(mp), intent(in) :: z
            real(mp) :: res
            res = product([(z+k, k=-mode,n-1-mode)]) / (z+m)
        end function
    end subroutine

end module