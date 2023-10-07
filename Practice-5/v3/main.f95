! Задание 5: Аппроксимация кубическими сплайнами

program quest5v3
    use my_io
    use my_math
    implicit none
    
    integer :: n
    real(mp), allocatable :: XYP(:,:), XY(:,:) ! массивы X, Y и P (весов)

    open(1, file='data.dat', status='old')
        read(1,'(2x, i5)') n ! количество интервалов (вершин - n+1)
        allocate(XYP(3, n+1))
        read(1,*) XYP
    close(1)
    call output('XYP =', XYP)

    allocate(XY(2, n+1))
    XY(:,:) = XYP(:2,:)

    call output('XY =', XY)

    open(1, file='result.dat')
        write(1,'(2f9.'//str(dp)//')') XY
    close(1)
    
end program