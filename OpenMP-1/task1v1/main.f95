module functions
    use my_io
    implicit none
    contains

    function f(x) result(y)
        real(mp) :: x, y
        y = x**2 - 1
    end function

end module

program openmp1_1v1
    use my_io
    use my_math
    use functions
    use omp_lib
    implicit none

    integer :: i, n, numt
    real(mp) :: a, b, alpha, beta, sum, h, s, h_small

    a = 1
    b = 10
    n = 100

    !$omp parallel 
      numt=omp_get_num_threads()
    !$omp end parallel 

    h = (b - a) / numt
    sum = 0

    !$omp parallel private(alpha, beta) reduction(+:sum)
        alpha = a + h * omp_get_thread_num()
        beta = alpha + h
        h_small = h / n
        s = 0
        do i = 1,n
            s = s + f(alpha+(i-0.5_mp)*h_small)
        end do
        s = s * h_small
        !write(*,'("num=", i0, " alpha=", f0.3)') omp_get_thread_num(), alpha
        sum = s !integrate(f, alpha, beta, n, 'rectangle')
    !$omp end parallel
    
    write(*,*) sum !324

end program