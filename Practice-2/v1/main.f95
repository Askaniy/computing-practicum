program quest2v1
    use my_io
    use my_math
    implicit none

    integer :: j, n, m, q
    real(mp) :: a, b
    real(mp), allocatable :: grid(:,:), interpolated(:,:)
    character(10) :: mode

    call get_command_argument(1, mode)
    write(*,*) '"'//mode//'"'

    call import_grid(trim(mode)//'.dat', grid, a, b, trim(mode))
    call output('grid = ', grid)

    n = size(grid, dim=2) - 1
    q = 50 ! шаг интерполяции
    
    interpolated = polynomial_interp(grid, q)
    m = size(interpolated, dim=2) - 1 ! q*n число интервалов интерполированной функции
    !call output('interpolated = ', interpolated)

    ! Растягивает отмасштабированную до [-1, 1] колонку аргументов на [a, b]
    interpolated(1, :) = (a + b + (b-a)*interpolated(1, :)) / 2.0_mp

    open(1, file='res_'//trim(mode)//'.dat')
        write(1,'("# ", i0)') m
        do j = 1,m+1
            write(1,'(2e11.'//str(dp)//')') interpolated(:, j)
        end do
    close(1)

end program