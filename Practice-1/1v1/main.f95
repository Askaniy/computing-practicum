program quest01_1v1
    use my_prec
    use my_io
    implicit none

    ! вариант с использованием my_io и неизвестными (меньше 100x100) размерами массива

    ! система координат не алгебраическая, а классическая: сначала X (i), потом -Y (j)
    ! к счастью, Фортран так и задуман

    integer :: i, j, k, n
    real, allocatable :: a(:,:), b(:,:), c(:,:)

    call input("введите массив a=", a)
    call input("введите массив b=", b)

    n = size(a, dim=1)
    allocate(c(n, n))
    c = 0

    forall (i=1:n, j=1:n, k=1:n)
        c(i, j) = c(i, j) + a(k, j) * b(i, k)
    end forall
    
    call output("a*b=", c)

    ! красиво и элегантно... а теперь вспоминаем про n=10^4

end program