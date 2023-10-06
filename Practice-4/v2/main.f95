program quest4v2
    use my_io
    use my_math
    implicit none

    integer :: n
    real(mp), allocatable :: a(:,:), b(:), x(:)
    character(:), allocatable :: mode

    ! Для модификации режима обработки надо вызывать make mode='jacobi', 'seidel' или 'relax'
    call read_argument(1, mode, default='relax')

    open(1, file='data.dat', status='old')
        read(1,'(2x, i5)') n
        allocate(a(n,n), b(n), x(n))
        read(1,*) a, b
    close(1)

    x = solve_sle(a, b)
    call output('Гаусс:', x)
    x = solve_diagdominant_sle(a, b, mode=mode)
    call output('Итеративно:', x)

    open(1, file='result.dat')
        write(1,'("# ", i0)') n
        write(1,'(f0.'//str(dp)//')') x
    close(1)

    write(*,*) 'Невязка:', dist(multiply(a, x), b)
    
end program