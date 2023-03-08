program quest01_1v3
    use my_io
    use my_math
    implicit none

    integer :: i, j, n
    real(mp), allocatable :: a(:,:), b(:,:), c(:,:)

    a = import_matrix('data1.dat', 'square')
    !call output('a=', a)

    b = import_matrix('data2.dat', 'square')
    !call output('b=', b)

    n = size(a, dim=1)

    !c = matmul(a, b) ! в моих координатах эта функция ломается
    c = multiply_matrix(a, b, 'square')
    !call output('a*b=', c)

    open(1, file='result.dat')
    write(1,'("# ", i0)') n
        do j = 1,n
            write(1,'('//str(n)//'e10.'//str(dp)//')') (c(i, j), i=1,n)
        end do
    close(1)

end program