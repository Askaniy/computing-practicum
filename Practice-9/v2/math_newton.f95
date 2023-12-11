
! Модуль реализации метода Ньютона

module math_newton
    use io_precision, only: mp
    use math_general, only: eps, sqrt_eps, multiply
    use math_sle, only: solve_sle
    implicit none

    private
    public newton
    
    integer, private :: i

    contains

    ! Многомерный метод Ньютона
    function newton(f, initial_vector, iterations_limit) result(x1)
        real(mp), intent(in) :: initial_vector(:)
        integer, intent(in), optional :: iterations_limit
        real(mp), dimension(size(initial_vector)) :: x0, x1
        real(mp), dimension(size(initial_vector), size(initial_vector)) :: jacobi_matrix
        integer :: limit
        interface
            pure function f(x) result(y)
                use io_precision, only: mp
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
                use io_precision, only: mp
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

end module