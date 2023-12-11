
! Модуль решения СЛУ

module math_sle
    use io_precision
    use math_general
    implicit none

    private
    public solve_sle, solve_diagdominant_sle, solve_pentadiagdominant_sle
    
    integer, private :: i, j, k

    contains

    ! Решение СЛУ методом Гаусса и его модификациями
    ! Режим определяется первой буквой из вариантов: gauss, jordan, mainch
    ! По умолчанию - выбор ведущего элемента (mainch)
    function solve_sle(a0, b0, mode0) result(x)
        real(mp), intent(in) :: a0(:,:), b0(:)
        real(mp) :: a(size(b0)+1,size(b0)), x(size(b0))
        integer :: n, lead
        character(*), optional :: mode0
        character(1) :: mode
        if (.not. present(mode0)) then
            mode = 'm' ! mainch
        else
            mode = mode0
        end if
        n = size(b0) ! размер СЛУ
        a(:n,:) = a0
        a(n+1,:) = b0
        do k = 1,n ! переход к треугольной матрице
            if (mode == 'm') then ! выбор ведущего элемента (mainch)
                lead =  maxloc(abs(a(k,k:) / maxval(a(:,k:), dim=1)), dim=1) + k-1
                if (lead /= k) then
                    call swap(a(:,k), a(:,lead))
                    !write(*,*) 'Строка '//str(lead)//' теперь ведущая'
                end if
            end if
            !if (abs(a(k,k)) < eps) write(*,*) 'Диагональный элемент итерации '//str(k)//' близок к нулю'
            a(k:,k) = a(k:,k) / a(k,k)
            do concurrent (j=k+1:n)
                a(k:,j) = a(k:,j) - a(k:,k) * a(k,j)
            end do
        end do
        if (mode == 'j') then ! jordan
            do concurrent (j=n:1:-1)
                a(n+1,1:j-1) = a(n+1,1:j-1) - a(j,1:j-1) * a(n+1,j)
            end do
            x = a(n+1, :)
        else ! схема с Гауссом или выбором
            x(n) = a(n+1,n)
            do concurrent (j=n-1:1:-1)
                x(j) = a(n+1,j) - dot_product(a(j+1:n,j), x(j+1:n))
            end do
        end if
    end function

    ! Решение СЛУ с диагональным преобладанием итерационными методами
    ! Режим определяется первой буквой из вариантов: jacobi, seidel, relax
    ! По умолчанию - метод релаксации (relax)
    function solve_diagdominant_sle(a, b, mode0) result(x)
        real(mp), intent(in) :: a(:,:), b(:)
        real(mp) :: x(size(b))
        integer :: n
        character(*), optional :: mode0
        character(1) :: mode
        if (.not. present(mode0)) then
            mode = 'r' ! relax
        else
            mode = mode0
        end if
        if (.not. isdiagdominant(a)) stop 'Матрица не имеет диагонального преобладания!'
        n = size(b) ! размер СЛУ
        select case (mode)
            case ('j') ! метод Якоби
                block
                    real(mp) :: x0(n), g(n), d_z(n,n), d_rev(n,n)
                    x0 = b ! произвольно
                    d_z = 0 ! матрица для хранения D и Z, сначала D (диагональ A)
                    d_rev = 0 ! матрица для хранения D^(-1)
                    do concurrent (j=1:n)
                        d_z(j,j) = a(j,j)
                        d_rev(j,j) = 1.0 / a(j,j)
                    end do
                    d_z = multiply(d_rev, d_z-a) ! теперь Z
                    g = multiply(d_rev, b)
                    do
                        x = matmul(d_z, x0) + g ! почему-то multiply не работает
                        if (dist(x0, x) < eps) exit
                        x0 = x
                    end do
                end block
            case ('s') ! метод Зейделя
                block
                    real(mp) :: x0(n), p_j(n)
                    x0 = b ! произвольно
                    do
                        do j=1,n
                            p_j = -a(:,j) / a(j,j)
                            x(j) = dot_product(p_j(1:j-1), x(1:j-1)) + dot_product(p_j(j+1:n), x0(j+1:n)) + b(j)/a(j,j)
                        end do
                        if (dist(x0, x) < eps) exit
                        x0 = x
                    end do
                end block
            case ('r') ! метод релаксации
                block
                    real(mp) :: p(n,n), q(n)
                    integer :: max_index
                    do j=1,n
                        p(:,j) = -a(:,j) / a(j,j)
                        q(j) = b(j) / a(j,j) ! вектор невязок
                    end do
                    x = 0 ! обязательно
                    do
                        max_index = maxloc(abs(q), dim=1)
                        x(max_index) = x(max_index) + q(max_index)
                        if (abs(q(max_index)) < eps) exit
                        q = q + p(max_index,:) * q(max_index)
                    end do
                end block
        end select
    end function

    ! Возвращает .true., если у матрицы имеется диагональное преобладание
    pure logical function isdiagdominant(a) 
        real(mp), intent(in) :: a(:,:)
        isdiagdominant = all([( ( 2*abs(a(j,j)) >= sum(abs(a(:,j))) ), j=1,size(a, dim=2) )])
    end function 

    ! Решение СЛУ методом пятиточечной прогонки
    ! Принимает пятидиагональную матрицу a0 в компактном виде, из пяти колонок
    function solve_pentadiagdominant_sle(a0, b0) result(x)
        real(mp), intent(in) :: a0(:,:), b0(:)
        real(mp) :: x(size(b0)), a(-1:size(b0)), b(-1:size(b0)), c(-1:size(b0)), &
                    alpha, beta, p(-1:size(b0)), q(-1:size(b0)), r(-1:size(b0))
        integer :: n
        n = size(b0) ! размер СЛУ
        a = 0
        b = 0
        c = 0
        a(1:) = a0(3,:) ! переобозначение колонок по условию задания
        b(1:) = a0(4,:) !     и расширение первых двух элементов нулями
        c(1:) = a0(5,:) !     для обращений по i-2 и i-1
        p = 0
        q = 0
        r = 0
        ! Прямой ход
        do i = 1,n
            beta = b(i-1) - p(i-2) * c(i-2)
            alpha = a(i) - p(i-1) * beta - q(i-2) * c(i-2)
            p(i) = (b(i) - q(i-1) * beta) / alpha
            q(i) = c(i) / alpha
            r(i) = (b0(i) - r(i-1) * beta - r(i-2) * c(i-2)) / alpha
        end do
        ! Обратный ход
        x(n) = r(n)
        x(n-1) = r(n-1) - p(n-1) * x(n)
        do i = n-2,1,-1
            x(i) = r(i) - p(i) * x(i+1) - q(i) * x(i+2)
        end do
    end function

    ! Сжимает пятидиагональную матрицу (не используется)
    function compressed(matrix)
        real(mp), intent(in) :: matrix(:,:)
        real(mp) :: compressed(5, size(matrix, dim=2))
        integer :: n
        n = size(matrix, dim=2)
        compressed = 0
        compressed(1, 3:n) = [( matrix(i,i+2), i=1,n-2 )]
        compressed(2, 2:n) = [( matrix(i,i+1), i=1,n-1 )]
        compressed(3, 1:n) = [( matrix(i,i),   i=1,n   )]
        compressed(4, 1:n-1) = [( matrix(i,i-1), i=2,n )]
        compressed(5, 1:n-2) = [( matrix(i,i-2), i=3,n )]
    end function

end module