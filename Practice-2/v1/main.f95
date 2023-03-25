program quest2v1
    use my_io
    use my_math
    implicit none

    integer :: j, n, m
    integer, parameter :: q = 50 ! шаг интерполяции
    real(mp) :: a, b
    real(mp), allocatable :: grid(:,:), interpolated(:,:)
    character(10) :: mode

    call get_command_argument(1, mode)
    write(*,*) '"'//mode//'"'

    call import_grid(trim(mode)//'.dat', grid, a, b, trim(mode))
    call output('grid = ', grid)

    n = size(grid, dim=2) - 1 ! число интервалов сетки

    ! Растягивает отмасштабированную до [-1, 1] колонку аргументов на [a, b]
    grid(1, :) = (a + b + (b-a)*grid(1, :)) / 2.0_mp
    
    interpolated = polynomial_interp(grid, q, a, b)
    m = size(interpolated, dim=2) - 1 ! m=q*n, число интервалов интерполированной функции

    open(1, file='res_'//trim(mode)//'.dat')
        write(1,'("# ", i0)') m
        do j = 1,m+1
            write(1,'(2e11.'//str(dp)//')') interpolated(:, j)
        end do
    close(1)

end program