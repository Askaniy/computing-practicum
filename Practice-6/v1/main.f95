! Задание 6: Многомерный метод Ньютона

module functions
    use my_io
    use my_math
    implicit none

    integer :: i, j, n
    real(mp) :: matrix3(3,3) = reshape([2, 1, 3, -1, 1, 0, -1, 1, 0], [3, 3])

    contains

    pure function test_func_1(x) result(y)
        real(mp), intent(in), dimension(:) :: x
        real(mp), dimension(size(x)) :: y
        do concurrent (i=1:size(x))
            y(i) = x(i)**i - 2 ! решения: (2, +-2^(1/2), 2^(1/3))
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

end module

program quest6v1
    use my_io
    use my_math
    use functions
    implicit none
    
    real(mp), allocatable :: initial_vector(:), jacobian(:,:)

    initial_vector = [11, 12, 13]
    call output('res =', test_func_3(initial_vector))

    !open(1, file='result.dat')
    !    write(1,'(f9.'//str(dp)//')') newton(test_func_3, initial_vector)
    !close(1)

    jacobian = differentiate(test_func_3, initial_vector)
    call output('Якобиан =', jacobian)
    
end program