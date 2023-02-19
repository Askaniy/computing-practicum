module my_math
    use my_prec
    use my_io
    implicit none
    contains

    function multiply_matrix(a, b, mode) result(c)
        real, intent(in) :: a(:,:), b(:,:)
        real, allocatable :: c(:,:)
        integer :: i, j, k, m
        character(*) :: mode
        m = size(a, dim=2)
        if (mode == 'square') then
            allocate(c(m, m))
            c = 0
            forall (i=1:m, j=1:m, k=1:m)
                c(i, j) = c(i, j) + a(k, j) * b(i, k)
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