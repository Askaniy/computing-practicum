module my_math
    use my_prec
    use my_io
    implicit none
    contains

    function square_matrix_multiply(a, b) result(c)
        real, intent(in) :: a(:,:), b(:,:)
        real, allocatable :: c(:,:)
        integer :: i, j, k, n
        n = size(a, dim=1)
        allocate(c(n, n))
        c = 0
        forall (i=1:n, j=1:n, k=1:n)
            c(i, j) = c(i, j) + a(k, j) * b(i, k)
        end forall
    end function

    function tridiagonal_matrix_multiply(a, b, m) result(c)
        real, dimension(3,0:m+1), intent(in) :: a, b
        real, dimension(5,m) :: c
        integer :: j, m
        c = 0
        do j=1,m
            c(1, j) = a(1, j) * b(1, j-1)
            c(2, j) = a(1, j) * b(2, j-1) + a(2, j) * b(1, j)
            c(3, j) = a(1, j) * b(3, j-1) + a(2, j) * b(2, j) + a(3, j) * b(1, j+1)
            c(4, j) = a(2, j) * b(3, j) + a(3, j) * b(2, j+1)
            c(5, j) = a(3, j) * b(3, j+1)
        end do
    end function

end module