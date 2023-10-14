! Задание 6: Многомерный метод Ньютона

program quest6v1
    use my_io
    use my_math
    implicit none
    
    integer :: i, j, n
    real(mp), allocatable :: solution(:)

    solution = [1, 2, 3]

    open(1, file='result.dat')
        write(1,'(f9.'//str(dp)//')') solution
    close(1)

    contains

    function test_func_1(x)
        real(mp), intent(in) :: x(:)
        real(mp) :: test_func_1(size(x))
        do concurrent (i=1:size(x))
            test_func_1(i) = x(i)**i - 2 ! решения: (2, +-2^(1/2), 2^(1/3))
        end do
    end function

    function test_func_2(x)
        real(mp), intent(in) :: x(:)
        real(mp) :: test_func_2(size(x)), matrix(size(x), size(x))
        n = size(x)
        ! генерация матрицы, однозначно определяемой размерностью
        do concurrent (i=1:n, j=1:n)
            matrix(i,j) = mod(i+j, n) + i - j
        end do
        matrix(:,n-1) = matrix(:,n) ! уменьшение ранга для возникновения нетривиального решения
        test_func_2 = multiply(matrix, x)
    end function
    
end program