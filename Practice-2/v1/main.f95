program quest2v1
    use my_io
    use my_math
    implicit none

    integer :: i, j, n, m, q
    real(mp) :: a, b
    real(mp), allocatable :: grid(:,:), interpolated(:,:)
    character(10) :: mode

    call get_command_argument(1, mode)
    write(*,*) '"'//mode//'"'

    call import_grid(trim(mode)//'.dat', grid, a, b, trim(mode))
    call output('grid = ', grid)

    n = size(grid, dim=2) - 1
    q = 5 ! шаг интерполяции
    
    interpolated = polynomial_interp(grid, q)
    m = size(interpolated, dim=2) - 1 ! q*n число интервалов интерполированной функции
    !call output('interpolated = ', interpolated)

    call unscale_grid(interpolated, a, b)
    !call output('interpolated = ', interpolated)

    open(1, file='res_'//trim(mode)//'.dat')
        write(1,'("# ", i0)') m
        do j = 1,m+1
            write(1,'(2e11.'//str(dp)//')') interpolated(:, j)
        end do
    close(1)

end program