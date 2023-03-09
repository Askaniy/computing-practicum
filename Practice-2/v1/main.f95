program quest2v1
    use my_io
    use my_math
    implicit none

    integer :: i, j, n, q
    real(mp) :: a, b
    real(mp), allocatable :: grid(:,:)

    call import_grid('uniform.dat', grid, a, b, 'uniform')
    call output('grid = ', grid)

    call unscale_grid(grid, a, b)
    call output('grid = ', grid)

    n = size(grid, dim=2)
    q = 100 ! шаг интерполяции

    open(1, file='res_uniform.dat')
    write(1,'("# ", i0)') q*n ! число интервалов интерполированной функции
        do j = 1,n+1
            write(1,'(2e10.'//str(dp)//')') grid(:, j)
        end do
    close(1)

end program