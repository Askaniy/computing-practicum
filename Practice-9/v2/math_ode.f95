
! Модуль поиска решения систем обыкновенных дифференциальных уравнений

module math_ode
    use io_precision, only: mp
    use math_general, only: factorial, diffs
    use math_quad, only: integrate
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
        real(mp) :: h, x(size(x0), size(t)), a(m)
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
        a = adams_extrap_coefficients(m)
        x = 0
        x(:,:m) = ode_runge_kutta_4(f, t(:m), x0) ! начальные данные
        call output('', a)
        do i=m+1,n
            x(:,i) = x(:,i-1) + h * dot_product(a, [(f(t(i-j), x(:,i-j)), j=1,m)])
        end do
        write(*,*) x
    end function

    function adams_extrap_coefficients(n) result(a)
        integer, intent(in) :: n
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
            call adams_extrap_integral(integral, n, j)
            a(j) = integral / a(j)
        end do
    end function

    ! Генерирует функцию и интегрирует её
    subroutine adams_extrap_integral(integral, n, m)
        integer, intent(in) :: n, m
        real(mp), intent(out) :: integral
        integral = integrate(f, 0._mp, 1._mp, 10, 'gauss')
        contains
        pure function f(z) result(res)
            use io_precision, only: mp
            real(mp), intent(in) :: z
            real(mp) :: res
            !res = product([(z+k, k=0,n-1)], k/=m)
            res = product([(z+k, k=0,n-1)]) / (z+m)
        end function
    end subroutine

    ! Интерполяционный метод Адамса
    function ode_adams_interp(t, x0) result(x)
        real(mp), intent(in) :: t(:), x0(:)
        real(mp) :: x(size(x0), size(t))
        call random_number(x)
    end function

end module