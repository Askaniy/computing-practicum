! Программа тестирует вторую часть задания, решение СЛУ методом пятиточечной подгонки

! Заранее вычисленный ответ получается таким: c = 3.754 2.762 2.657 6.315 7.701

program quest5v2
    use my_io
    use my_math
    implicit none
    
    real(mp), allocatable :: a(:,:), b(:), c(:)

    a = import_matrix('data1.dat', 'square')
    call output('a =', a)

    b = import_vector('data2.dat')
    call output('b =', b)

    c = solve_diagdominant_sle(a, b, 'relax')
    call output('reference =', c)

    c = solve_pentadiagdominant_sle(compressed(a), b)
    call output('result =', c)
    
end program