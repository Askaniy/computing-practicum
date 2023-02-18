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

end module