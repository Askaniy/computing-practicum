program quest01_1v2
    use my_prec
    use my_io
    use my_math
    implicit none

    integer :: i, j, n
    real, allocatable :: a(:,:), b(:,:), c(:,:)

    open(1, file='data1.dat', status='old')
        read(1,'(2x, i5)') n
        allocate(a(n, n))
        do j = 1,n
            read(1,*) (a(i, j), i=1,n)
        end do
    close(1)
    call output("a=", a)

    open(2, file='data2.dat', status='old')
        read(2,'(2x, i5)') n
        allocate(b(n, n))
        do j = 1,n
            read(2,*) (b(i, j), i=1,n)
        end do
    close(2)
    call output("b=", b)

    !c = matmul(a, b) ! в моих координатах эта функция ломается
    c = square_matrix_multuply(a, b)
    call output("a*b=", c)

    open(3, file='result.dat')
        write(3,*) '# '//str(n)
        do j = 1,n
            write(3,'('//str(n)//'e11.'//str(md)//')') (c(i, j), i=1,n)
        end do
    close(3)

end program