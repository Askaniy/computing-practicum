program quest01_1v2
    use my_prec
    use my_io
    use my_math
    implicit none

    integer :: i, j, k, n
    real, allocatable :: a(:,:), b(:,:), c(:,:)
    character(10) :: line

    open(1, file='data1.dat', status='old')
    read(1,*) line
    write(*,*) line ! сбоит на этом месте -- я ожидаю, что в result запишется # 3, а там только #
    read(line(2:),*) n
    allocate(a(n, n))
    do i = 1,n
        read(1,*) (a(i, j), j=1,n)
    end do
    close(1)
    call output("a=", a)

    open(2, file='data2.dat', status='old')
    read(2,*) line
    allocate(b(n, n))
    do i = 1,n
        read(2,*) (b(i, j), j=1,n)
    end do
    close(2)
    call output("b=", b)

    !allocate(c(n, n))
    !c = 0

    !forall (i=1:n, j=1:n, k=1:n)
    !    c(i, j) = c(i, j) + a(k, j) * b(i, k)
    !end forall
    
    !call output("a*b=", c)

end program