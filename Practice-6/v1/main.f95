! Задание 6: Многомерный метод Ньютона

module functions
    use my_io
    use my_math
    use my_consts
    implicit none

    integer :: i, j, n
    real(mp) :: matrix3(3,3) = reshape([2, 1, 3, -1, 1, 0, -1, 1, 0], [3, 3])

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

    function test_func_3(x) result(y) ! частный случай test_func_2 для размерности 3, тоже не работает
        real(mp), intent(in), dimension(3) :: x
        real(mp), dimension(3) :: y
        y = multiply(matrix3, x)
    end function

end module

program quest6v1
    use my_io
    use my_math
    use my_consts
    use functions
    implicit none
    
    real(mp), allocatable :: initial_vector(:)

    initial_vector = [10, 10, 10]
    call output('res =', test_func_3(initial_vector))
    write(*,*) size(test_func_3(initial_vector))
    !open(1, file='result.dat')
    !    write(1,'(f9.'//str(dp)//')') newton(test_func_3, initial_vector)
    !close(1)
    call output('Якобиан =', differentiate(test_func_3, initial_vector))
    
end program