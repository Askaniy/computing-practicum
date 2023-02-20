program quest01_1v2
    use my_prec
    use my_io
    use my_math
    implicit none

    integer :: i, j, m
    real(mp), allocatable :: a(:,:), b(:,:), c(:,:)

    a = import_matrix('data1.dat', 'tridiagonal')
    !call output('a=', a)

    b = import_matrix('data2.dat', 'tridiagonal')
    !call output('b=', b)

    m = size(a, dim=2)

    c = multiply_matrix(a, b, 'tridiagonal')
    !call output("a*b=", c)

    open(1, file='result.dat')
        write(1,'("# ", i0)') m
        write(1,'(5e10.'//str(md)//')') (c(i, 1), i=3,5)
        write(1,'(5e10.'//str(md)//')') (c(i, 2), i=2,5)
        do j = 3,m-2
            write(1,'(5e10.'//str(md)//')') (c(i, j), i=1,5)
        end do
        write(1,'(5e10.'//str(md)//')') (c(i, m-1), i=1,4)
        write(1,'(5e10.'//str(md)//')') (c(i, m), i=1,3)
    close(1)

end program