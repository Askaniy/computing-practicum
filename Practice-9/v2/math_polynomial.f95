
! Модуль поиска корней полиномов

module math_polynomial
    use io_precision, only: mp
    use math_general, only: eps
    implicit none

    private
    public solve_polynomial, solve_quadratic_equation
    
    integer, private :: i

    contains

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

end module