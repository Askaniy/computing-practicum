! Программа тестирует вторую часть задания, импорт и умножение пятидиагональных матриц

! Заранее вычисленный ответ получается таким:
! 36  28  10  14  12
! 84  106 90  32  38
! 94  52  94  56  32
! 62  76  84  68  88
! 48  118 76  22  58

program quest5v1
    use my_io
    use my_math
    implicit none
    
    integer :: i, j, m
    real(mp), allocatable :: a(:,:), b(:,:), c(:,:)

    a = import_matrix('data1.dat', 'pentadiagonal')
    call output('a =', a)

    b = import_matrix('data2.dat', 'pentadiagonal')
    call output('b =', b)

    m = size(a, dim=2)

    c = multiply(a, b, 'pentadiagonal')
    call output("a*b=", c)
    
end program