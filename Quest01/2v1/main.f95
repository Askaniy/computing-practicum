program quest01_1v2
    use my_prec
    use my_io
    use my_math
    implicit none

    integer :: i, j, m
    real, allocatable :: a(:,:), b(:,:), c(:,:)

    open(1, file='data1.dat', status='old')
        read(1,'(2x, i5)') m
        allocate(a(3, 0:m+1)) ! в расчётах буду ссылаться на строку выше и ниже
        a = 0                 ! на Питоне бы сделал парсер ошибок, но тут костыль проще
        read(1,*) (a(i, 1), i=2,3)
        do j = 2,m-1
            read(1,*) (a(i, j), i=1,3)
        end do
        read(1,*) (a(i, m), i=1,2)
    close(1)
    call output("a=", a)

    open(2, file='data2.dat', status='old')
        read(2,'(2x, i5)') m
        allocate(b(3, 0:m+1))
        b = 0
        read(2,*) (b(i, 1), i=2,3)
        do j = 2,m-1
            read(2,*) (b(i, j), i=1,3)
        end do
        read(2,*) (b(i, m), i=1,2)
    close(2)
    call output("b=", b)

    c = tridiagonal_matrix_multiply(a, b, m)
    call output("a*b=", c)

    open(3, file='result.dat')
        write(3,*) '# '//str(m)
        do j = 1,m
            write(3,'(5e11.'//str(md)//')') (c(i, j), i=1,5)
        end do
    close(3)

end program