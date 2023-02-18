module my_math
    use my_prec
    use my_io
    implicit none
    contains

    function square_matrix_multuply(a, b) result(c)
        real, intent(in) :: a(:,:), b(:,:)
        real, allocatable :: c(:,:)
        integer :: n
        n = size(a, dim=1)
        allocate(c(n, n))
        c = matmul(a, b) ! временно
    end function

end module