module game_of_life
    use omp_lib
    implicit none

    contains

    subroutine next_frame(matrix, cols, rows)
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
        !$omp parallel do collapse(1) schedule(guided)
            do j = 1,rows
                do i = 1,cols
                    count = sum(temp_matrix(i-1:i+1, j-1:j+1)) - temp_matrix(i, j)
                    if (count == 3) then
                        matrix(i, j) = 1
                    else if (temp_matrix(i, j) == 1 .and. count == 2) then
                        matrix(i, j) = 1
                    end if
                end do
            end do
        !$omp end parallel do
    end subroutine

end module
