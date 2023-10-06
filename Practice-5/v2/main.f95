! Программа тестирует вторую часть задания, метод пятиточечной подгонки

! Заранее вычисленный ответ получается таким:

program quest5v2
    use my_io
    use my_math
    implicit none
    
    integer :: i, j, m
    real(mp), allocatable :: a(:,:), b(:,:), c(:,:)

    a = import_matrix('data1.dat', 'pentadiagonal')
    call output('a =', a)

    b = import_matrix('data2.dat', 'vector')
    call output('b =', b)

    m = size(a, dim=2)

    c = multiply(a, b, 'pentadiagonal')
    call output("a*b=", c)
    
end program