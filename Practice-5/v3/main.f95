! Задание 5: Аппроксимация кубическими сплайнами

program quest5v3
    use my_io
    use my_math
    implicit none
    
    integer :: n, q=10 ! число разбиений интервала
    real(mp), allocatable :: XYP(:,:), XY(:,:) ! массивы X, Y и P (весов)

    open(1, file='data.dat', status='old')
        read(1,'(2x, i5)') n ! количество интервалов (узлов - n+1)
        allocate(XYP(3, n+1))
        read(1,*) XYP
    close(1)
    !call output('XYP =', XYP)

    XYP(3,:) = XYP(3,:) / 10000

    allocate(XY(2, n+1))
    XY = spline_approx(XYP, q)

    open(1, file='result.dat')
        write(1,'(2f9.'//str(dp)//')') XY
    close(1)
    
end program