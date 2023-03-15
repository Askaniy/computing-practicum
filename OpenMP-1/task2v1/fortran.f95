module game_of_life
    use omp_lib
    implicit none

    contains

    subroutine next_frame(matrix, new_matrix, cols, rows)
        integer :: i, j, count, cols, rows, numt, h, column, alpha, beta
        integer, intent(in) :: matrix(cols, rows)
        integer, intent(out) :: new_matrix(cols, rows)
        !$omp parallel
            numt = omp_get_num_threads()
        !$omp end parallel
        h = cols / numt
        !$omp parallel default(firstprivate) shared(new_matrix)
            column = omp_get_thread_num()
            alpha = 1 + column * h 
            beta = (1 + column) * h
            do i = alpha,beta
                do j = 1,rows
                    count = sum(matrix(max(1, i-1):min(cols, i+1), max(1, j-1):min(rows, j+1))) - matrix(i, j)
                    if (matrix(i, j) == 1) then
                        if (count < 2 .or. count > 3) then
                            new_matrix(i, j) = 0
                        else
                            new_matrix(i, j) = 1
                        end if
                    else
                        if (count == 3) then
                            new_matrix(i, j) = 1
                        end if
                    end if
                end do
            end do
        !$omp end parallel
    end subroutine

end module
