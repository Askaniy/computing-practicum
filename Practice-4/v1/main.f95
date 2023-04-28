program quest4v1
    use my_io
    use my_math
    implicit none

    integer :: n
    real(mp), allocatable :: a(:,:), b(:), x(:)
    character(:), allocatable :: mode

    ! Для модификации режима обработки надо вызывать make mode='jacobi', 'seidel' или 'relax'
    call read_argument(1, mode, default='jacobi')

    open(1, file='data.dat', status='old')
        read(1,'(2x, i5)') n
        allocate(a(n,n), b(n), x(n))
        read(1,*) a, b
    close(1)

    x = solve_sle(a, b, mode=mode)
    call output('X =', x) ! 1 2 2 0

    open(1, file='result.dat')
        write(1,'("# ", i0)') n
        write(1,'(f0.'//str(dp)//')') x
    close(1)

    write(*,*) 'Невязка:', sqrt(sum(multiply(a, x) - b)**2)

    write(*,*) isdiagdominant(a)
    
end program