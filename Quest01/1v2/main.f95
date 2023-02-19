program quest01_1v2
    use my_prec
    use my_io
    use my_math
    implicit none

    integer :: i, j, n
    real(mp), allocatable :: a(:,:), b(:,:), c(:,:)

    a = import_tarakanov('data1.dat')
    !call output('a=', a)

    b = import_tarakanov('data2.dat')
    !call output('b=', b)

    n = size(a, dim=1)

    !c = matmul(a, b) ! в моих координатах эта функция ломается
    c = square_matrix_multiply(a, b)
    !call output('a*b=', c)

    open(1, file='result.dat')
        write(1,*) '# '//str(n)
        do j = 1,n
            write(1,'('//str(n)//'e11.'//str(md)//')') (c(i, j), i=1,n)
        end do
    close(1)

end program