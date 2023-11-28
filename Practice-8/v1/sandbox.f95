module sandbox
    implicit none

    contains

    ! Делит `P(x) = a_0 x^n + ... + a_n` на `x - x0` методом Горнера
    function polynomial_division(a, x0) result(b)
        real(4), intent(in) :: a(0:), x0
        real(4) :: b(0:size(a)-2)
        integer :: n, i
        n = size(a) - 1 ! степень многочлена
        b(0) = a(0)
        do i = 1,n-1
            b(i) = b(i-1) * x0 + a(i)
        end do
        !r = b(n-1) * x0 + a(n) ! остаток
    end function

    ! Находит вещественные корни полинома `P(x) = a_0 x^n + ... + a_n = 0`
    function solve_polynomial(a) result(x)
        real(4), intent(in) :: a(0:)
        real(4) :: x1, x(size(a)-1), a1(size(a)-1)
        integer :: n, i
        n = size(a) - 1 ! степень многочлена
        write(*,*) n
        select case (n)
            case (1)
                x(1) = - a(1) / a(0)
            case (2)
                x(:) = solve_quadratic_equation(a(0), a(1), a(2))
            case (3:)
                !x1 = get_abs_max_root(a)
                !a1 = polynomial_division(a, x1)
                do i = n,2,-1
                    write(*,*) i
                end do
        end select
        x = 0
    end function

    ! Решает квадратное уравнение. Возвращает NaN, если корни комплексные.
    function solve_quadratic_equation(a, b, c) result(x)
        real(4), intent(in) :: a, b, c
        real(4) :: a2, x(2), left_part, right_part
        a2 = 2 * a
        left_part = -b / a2
        right_part = sqrt(b*b - 2*a2*c) / a2
        x(1) = left_part + right_part
        x(2) = left_part - right_part
    end function

    ! Поиск максимального по модулю корня через рекуррентное уравнение методом Бернулли
    function get_abs_max_root(a) result(x1)
        real(4), intent(in) :: a(0:)
        real(4) :: x1, y(1:size(a)-1), b(1:size(a)-1)
        integer :: n
        n = size(a) - 1 ! степень многочлена
        b = a(1:) / a(0) ! приведённые коэффициенты
        call random_number(y) ! n начальных значений
        do while (abs(y(1)/y(2) - y(2)/y(3)) > 0.0001) ! УТОЧНИТЬ EPSILON
            x1 = - dot_product(y, b) ! x1 как буферная переменная
            y(2:n) = y(1:n-1) ! сдвиг: новый элемент в начало
            y(1) = x1
            write(*,*) x1
        end do
        x1 = y(1)/y(2)
    end function

end module

program main
    use sandbox
    implicit none
    
    real(4) :: x, a(5) = [1, -3, -3, 7, 6] ! корни: -1, 2, 3

    !write(*,*) polynomial_division(a, 3.)
    
    !x = get_abs_max_root(a)
    !write(*,*) x

    !write(*,*) solve_quadratic_equation(2., -4., -5.)

    write(*,*) solve_polynomial(a)
    
end program