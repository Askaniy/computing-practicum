! Задание 8: Гауссова квадратура

program quest8v2
    use my_io
    use my_math
    use my_consts
    implicit none
    
    integer :: i, j, n
    real(mp), allocatable :: array(:,:)
    real(mp) :: a(5) = [1, -3, -3, 7, 6] ! корни: -1, 2, 3

    write(*,*) solve_polynomial(a)

    !open(1, file='data.dat', status='old')
    !    read(1,'(2x, i5)') n
    !    allocate(array(2, n+1))
    !    read(1,*) array
    !close(1)
    !call output('array =', array)
!
    !open(1, file='result.dat')
    !    write(1,'(2f9.'//str(dp)//')') array
    !close(1)
    
end program