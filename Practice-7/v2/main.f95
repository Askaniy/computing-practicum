! Задание 7: Быстрое преобразование Фурье

program quest7v2
    use my_io
    use my_math
    use my_consts
    implicit none
    
    integer :: i, n
    complex(mp), allocatable :: array(:), result(:)
    complex(mp) :: test(2,2)

    test = reshape([(20, 30), (400, 50), (6, 7), (8, 9)], [2, 2])
    call output('test =', test)

    !open(1, file='data.dat', status='old')
    !    read(1,'(2x, i5)') n
    !    allocate(array(n))
    !    read(1,*) array
    !close(1)
!
    !call output('a=', array)
!
    !result = array
!
    !open(1, file='result.dat')
    !    write(1,*) size(result)
    !    do i=1,size(result)
    !        write(1,'(2f9.'//str(dp)//')') real(result(i)), aimag(result(i))
    !    end do
    !close(1)
!
    !open(1, file='abs.dat')
    !    write(1,'(1f9.'//str(dp)//')') abs(result)
    !close(1)
    
end program