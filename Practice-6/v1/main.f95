! Задание 6: Многомерный метод Ньютона

module functions
    use my_io
    use my_math
    implicit none

    integer :: i, j, n
    real(mp) :: matrix3(3,3) = reshape([2, 1, 3, -1, 1, 1, -1, 1, 0], [3, 3])

    contains

    pure function test_func_1(x) result(y)
        real(mp), intent(in), dimension(:) :: x
        real(mp), dimension(size(x)) :: y
        do concurrent (i=1:size(x))
            y(i) = x(i)**i - 2 ! решения: [2, +-2^(1/2), 2^(1/3)]
        end do
    end function

    pure function test_func_2(x) result(y)
        real(mp), intent(in), dimension(:) :: x
        real(mp) :: y(size(x)), matrix(size(x), size(x))
        integer :: n
        n = size(x)
        ! генерация матрицы, однозначно определяемой размерностью
        do concurrent (i=1:n, j=1:n)
            matrix(i,j) = mod(i+j, n) + i - j
        end do
        matrix(:,n-1) = matrix(:,n) ! уменьшение ранга для возникновения нетривиального решения
        y = multiply(matrix, x)
    end function

    pure function test_func_3(x) result(y) ! частный случай test_func_2 для размерности 3
        real(mp), intent(in), dimension(:) :: x
        real(mp), dimension(size(x)) :: y
        y = multiply(matrix3, x)
    end function

    pure function test_func_4(x) result(y) ! решения: [0, -1, -1], [0, 1, 1]
        real(mp), intent(in), dimension(:) :: x
        real(mp), dimension(size(x)) :: y
        y(1) = x(1)*x(1) + x(2)*x(2) - 1
        y(2) = x(2)*x(2) - x(3)*x(3)
        y(3) = x(1) - x(2) + x(3)
    end function

end module

program quest6v1
    use my_io
    use my_math
    use functions
    implicit none
    
    real(mp), allocatable :: initial_vector(:), solution(:)

    initial_vector = [20, 20, 20]

    solution = newton(test_func_4, initial_vector)
    call output('решение X =', solution)
    call output('|F(X)| =', length(test_func_4(solution)))

    open(1, file='result.dat')
        write(1,'(f9.'//str(dp)//')') solution
    close(1)
    
end program