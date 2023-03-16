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
                    end if
                end do
            end do
        !$omp end parallel
    end subroutine

    subroutine next_frame2(matrix, cols, rows)
        integer :: i, j, count, cols, rows
        integer(1), intent(in out) :: matrix(cols, rows)
        !f2py intent(in,out) :: matrix
        integer(1) :: temp_matrix(0:cols+1, 0:rows+1)
        ! создаём расширенную копию исходной матрицы
        temp_matrix(1:cols, 1:rows) = matrix
        temp_matrix(0, 0) = matrix(cols, rows)
        temp_matrix(0, 1:rows) = matrix(cols, :)
        temp_matrix(0, rows+1) = matrix(cols, 1)
        temp_matrix(1:cols, rows+1) = matrix(:, 1)
        temp_matrix(cols+1, rows+1) = matrix(1, 1)
        temp_matrix(cols+1, 1:rows) = matrix(1, :)
        temp_matrix(cols+1, 0) = matrix(1, rows)
        temp_matrix(1:cols, 0) = matrix(:, rows)
        ! обновляем на её основе исходную матрицу
        matrix = 0
        do i = 1,cols
            do j = 1,rows
                count = sum(temp_matrix(i-1:i+1, j-1:j+1)) - temp_matrix(i, j)
                if (count == 3) then
                    matrix(i, j) = 1
                else if (temp_matrix(i, j) == 1 .and. count == 2) then
                    matrix(i, j) = 1
                end if
            end do
        end do
    end subroutine

end module
