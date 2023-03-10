program quest2v1
    use my_io
    use my_math
    implicit none

    integer :: i, j, n, m, q
    real(mp) :: a, b
    real(mp), allocatable :: grid(:,:), interpolated(:,:)

    call import_grid('uniform.dat', grid, a, b, 'uniform')
    call output('grid = ', grid)

    n = size(grid, dim=2)
    q = 5 ! шаг интерполяции
    m = q*n ! число интервалов интерполированной функции
    
    interpolated = polynomial_interp(grid, q)
    call output('interpolated = ', interpolated)

    call unscale_grid(interpolated, a, b)
    call output('interpolated = ', interpolated)

    open(1, file='res_uniform.dat')
        write(1,'("# ", i0)') m
            do j = 1,m+1
                write(1,'(2e10.'//str(dp)//')') interpolated(:, j)
            end do
    close(1)

end program