! Задание 6: Многомерный метод Ньютона

program quest6v1
    use my_io
    use my_math
    implicit none
    
    integer :: i, j, n
    real(mp), allocatable :: array(:,:)

    open(1, file='data.dat', status='old')
        read(1,'(2x, i5)') n
        allocate(array(2, n+1))
        read(1,*) array
    close(1)
    call output('array =', array)

    open(1, file='result.dat')
        write(1,'(2f9.'//str(dp)//')') array
    close(1)

    contains

    function f(x):
        real(mp), intent(in) :: x(:)
        real(mp) :: f(size(n))
        do concurrent (i=1,size(n))
            f(i) = x**i - 2
        end do
    end function
    
end program