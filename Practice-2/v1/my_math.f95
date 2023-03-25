module my_math
    use my_io !use, intrinsic :: ieee_arithmetic
    implicit none

    integer, private :: i, j, k

    contains

    ! Возвращает интерполированный массив из сеточной функции и числа разбиений
    ! Представление в виде двух колонок, x и f(x)
    function polynomial_interp(grid, q, a, b) result(interpolated)
        integer, intent(in) :: q
        integer :: n, m
        real(mp), intent(in) :: grid(:, 0:), a, b
        real(mp) :: interpolated(2, 0:(size(grid, dim=2)-1)*q)
        real(mp) :: lagrange_basis, numerator, denominator
        
        n = size(grid, dim=2) - 1
        m = size(interpolated, dim=2) - 1 ! число интервалов
        
        interpolated(1, :) = [(a + j*(b-a)/m, j=0,m)]
        
        do j = 0,m
            interpolated(2, j) = 0.0_mp
            do k = 0,n
                numerator = 1.0_mp
                denominator = 1.0_mp
                do i = 0,n
                    if (i /= k) then
                        numerator = numerator * (interpolated(1, j) - grid(1, i))
                        denominator = denominator * (grid(1, k) - grid(1, i))
                    end if
                end do
                lagrange_basis = numerator / denominator
                interpolated(2, j) = interpolated(2, j) + grid(2, k) * lagrange_basis
            end do
        end do
    end function
    
    function integrate(f, a, b, n, mode) result(s)
        integer :: n, m
        real(mp), external :: f
        real(mp) :: s, a, b, h
        character(*) :: mode
        h = (b - a) / n
        if (mode == 'rectangle') then
            s = 0
            do i = 1,n
                s = s + f(a+(i-0.5_mp)*h)
            end do
            s = s * h
        elseif (mode == 'trapeze') then
            s = (f(a) + f(b)) / 2
            do i = 2,n
                s = s + f(a+(i-1)*h)
            end do
            s = s * h
        elseif (mode == 'simpson') then
            m = n
            if (mod(n, 2) == 1) then !s = ieee_value(s, ieee_quiet_nan); return
                m = n + 1
            end if
            s = simpson(f, a, b, m)
        end if
    end function

    function simpson(f, a, b, n) result(s)
        integer :: n
        real(mp) :: s, s1, s2, a, b, h, f, x
        h = (b - a) / n
        s1 = 0
        s2 = -f(b)
        do i = 2,n,2
            x = a+(i-1)*h
            s1 = s1 + f(x)
            s2 = s2 + f(x+h)
        end do
        s = h/3 * (f(a) + f(b) + 4*s1 + 2*s2)
    end function

    function multiply_matrix(a, b, mode) result(c)
        integer :: m
        real, intent(in) :: a(:,:), b(:,:)
        real, allocatable :: c(:,:)
        character(*) :: mode
        m = size(a, dim=2)
        if (mode == 'square') then
            allocate(c(m, m))
            forall (i=1:m, j=1:m)
                c(i, j) = dot_product(a(:,j), b(i,:))
            end forall
        elseif (mode == 'tridiagonal') then
            allocate(c(5, m))
            c = 0
            c(1:2, 1) = 0 ! NaN
            c(3, 1) = a(2, 1) * b(2, 1) + a(3, 1) * b(1, 2)
            c(4, 1) = a(2, 1) * b(3, 1) + a(3, 1) * b(2, 2)
            c(5, 1) = a(3, 1) * b(3, 2)
            do j=2,m-1
                c(1, j) = a(1, j) * b(1, j-1)
                c(2, j) = a(1, j) * b(2, j-1) + a(2, j) * b(1, j)
                c(3, j) = a(1, j) * b(3, j-1) + a(2, j) * b(2, j) + a(3, j) * b(1, j+1)
                c(4, j) = a(2, j) * b(3, j) + a(3, j) * b(2, j+1)
                c(5, j) = a(3, j) * b(3, j+1)
            end do
            c(1, m) = a(1, m) * b(1, m-1)
            c(2, m) = a(1, m) * b(2, m-1) + a(2, m) * b(1, m)
            c(3, m) = a(1, m) * b(3, m-1) + a(2, m) * b(2, m)
            c(4:5, m) = 0 ! NaN
        end if
    end function

end module