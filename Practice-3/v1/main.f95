program quest3v1
    use my_io
    use my_math
    implicit none

    integer :: n, i
    real(mp), allocatable :: a(:,:), b(:), x(:)
    character(:), allocatable :: mode

    call read_argument(1, mode)

    open(1, file='data.dat', status='old')
        read(1,'(2x, i5)') n
        allocate(a(n,n), b(n), x(n))
        read(1,*) a, b
    close(1)
    !call output('A =', a)
    !call output('B =', b)

    x = solve_sle(a, b, mode=mode)
    call output('X =', x)
    !matrix(0.2187202664129401 0.8013796384395814 0.2724785918173168 -0.2847288296860133 0.3342055185537584)

    open(1, file='result.dat')
        write(1,'("# ", i0)') n
        write(1,'(f0.'//str(dp)//')') (x(i), i=1,n)
    close(1)
    
end program