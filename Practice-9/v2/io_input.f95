
! Модуль процедур ввода

module io_input
    use io_precision, only: mp
    use io_general, only: str, str_max
    use io_strings, only: isspace, isspacesymbol
    implicit none

    private
    public import_vector, import_matrix, input

    integer, parameter :: y_max = 100 ! максимальное количество строк в импортируемой 2D матрице

    interface input
        module procedure input_char, input_int, input_real, input_int1D, input_int2D, input_real1D, input_real2D
    end interface

    contains

    ! Специализированное чтение вектора, у которого указан размер
    function import_vector(path) result(array)
        integer :: n
        real(mp), allocatable :: array(:)
        character(*) :: path
        open(1, file=path, status='old')
            read(1,'(2x, i5)') n ! размер матрицы задаётся с третьего символа первой строки
            allocate(array(n))
            read(1,*) array
        close(1)
    end function

    ! Специализированное чтение матрицы, у которой указан размер
    function import_matrix(path, mode) result(array)
        integer :: i, j, n
        real(mp), allocatable :: array(:,:)
        character(*) :: path
        character(*), optional :: mode
        if (.not. present(mode)) then
            mode = 'square'
        end if
        open(1, file=path, status='old')
            read(1,'(2x, i5)') n ! размер матрицы задаётся с третьего символа первой строки
            if (mode == 'square') then
                allocate(array(n, n))
                read(1,*) array
            elseif (mode == 'tridiagonal') then
                allocate(array(3, n))
                array = 0
                read(1,*) (array(i, 1), i=2,3) ! первая строчка диагонали вылезает за пределы матрицы на один элемент
                do j = 2,n-1
                    read(1,*) array(:, j)
                end do
                read(1,*) (array(i, n), i=1,2) ! последняя - тоже
            elseif (mode == 'pentadiagonal') then
                allocate(array(5, n))
                array = 0
                read(1,*) (array(i, 1), i=3,5) ! первая строчка диагонали вылезает за пределы матрицы на два элемента
                read(1,*) (array(i, 2), i=2,5) ! вторая - на один
                do j = 3,n-2
                    read(1,*) array(:, j)
                end do
                read(1,*) (array(i, n-1), i=1,4) ! предпоследняя - тоже на один
                read(1,*) (array(i, n), i=1,3) ! последняя - снова на два
            end if
        close(1)
    end function


    ! Серия подпрограмм input. Реализует ввод и контрольную печать целых/вещественных переменных и 1D/2D массивов
    ! Пример использования: call input("n = ", n)
    ! Конец 2D массива определяется концом файла или пробельной строкой (см. функцию isspace)

    subroutine input_char(text, v)
        character(*), intent(in) :: text
        character(*), intent(out) :: v
        integer :: ios
        write(*,'(a$)') text
        read(*,*,iostat=ios) v
        if (ios > 0) then
            write(*,*) 'Ошибка при чтении строки. Индекс ошибки '//str(ios)
            stop 1
        endif
        write(*,*) 'Получено "'//v//'"'
    end subroutine

    subroutine input_int(text, v)
        character(*), intent(in) :: text
        integer, intent(out) :: v
        integer :: ios
        write(*,'(a$)') text
        read(*,*,iostat=ios) v
        if (ios > 0) then
            write(*,*) 'Ошибка при чтении целого числа. Индекс ошибки '//str(ios)
            stop 1
        endif
        write(*,'("Получено ", i0)') v
    end subroutine

    subroutine input_real(text, v)
        character(*), intent(in) :: text
        real(mp), intent(out) :: v
        integer :: ios
        write(*,'(a$)') text
        read(*,*,iostat=ios) v
        if (ios > 0) then
            write(*,*) 'Ошибка при чтении вещ. числа. Индекс ошибки '//str(ios)
            stop 1
        endif
        write(*,*) 'Получено', v
    end subroutine

    subroutine read_int1D(v, ios)
        integer, allocatable, intent(out) :: v(:)
        integer, intent(out) :: ios
        character :: buffer(str_max), c
        character(16) :: string
        integer :: ncol, i, j, numbers(str_max)
        logical :: lastisspace
        numbers = 0
        j = 1
        ncol = 0
        lastisspace=.true.
        do i=1,str_max
            read(*, '(a)', advance='no', iostat=ios) c
            if (ios /= 0) then
                write(string,*) buffer(j:i-1)
                !write(*,*) "ios="//str(ios)
                if (isspace(string)) then
                    ios = -3
                    !write(*,*) "new ios="//str(ios)
                else
                    read(string,*) numbers(ncol)
                    !write(*,*) numbers(ncol)
                end if
                exit
            end if
            buffer(i) = c
            if (lastisspace .and. .not. isspacesymbol(c)) then
                if (ncol /= 0) then
                    write(string,*) buffer(j:i-2)
                    read(string,*) numbers(ncol)
                    !write(*,*) numbers(ncol)
                end if
                ncol = ncol + 1
                j = i
            end if
            lastisspace = isspacesymbol(c)
        end do
        allocate(v(ncol))
        v = numbers(1:ncol)
    end subroutine

    subroutine input_int1D(text, v)
        character(*), intent(in) :: text
        integer, allocatable, intent(out) :: v(:)
        integer :: ios
        write(*,'(a$)') text
        call read_int1D(v, ios)
        call output_int1D('Получено', v)
    end subroutine

    subroutine read_int2D(v)
        integer, allocatable, intent(out) :: v(:,:)
        integer, allocatable :: w(:)
        integer, allocatable :: buffer(:,:)
        integer :: ios, sz_x, sz_y, i
        i = 0
        do
            i = i + 1
            call read_int1D(w, ios)
            if (ios == -3) exit
            !call output("Строка "//str(i)//" ", w)
            if (i == 1) then
                sz_x = size(w)                ! Определяем размер массива по первой строчке
                allocate(buffer(sz_x, y_max)) ! (ранее было по максимальной, но стало давать ошибку)
            end if                            ! Array bound mismatch for dimension 1 of array 'buffer' (100/4)
            buffer(:,i) = w(:)
            if (ios == -1) then
                i = i + 1
                exit
            end if
        end do
        sz_y = i - 1
        allocate(v(sz_x, sz_y))
        v = buffer(1:sz_x, 1:sz_y)
    end subroutine

    subroutine input_int2D(text, v)
        character(*), intent(in) :: text
        integer, allocatable, intent(out) :: v(:,:)
        write(*,'(a)') text
        call read_int2D(v)
        call output_int2D('Получено', v)
    end subroutine

    subroutine read_real1D(v, ios)
        real(mp), allocatable, intent(out) :: v(:)
        integer, intent(out) :: ios
        character :: buffer(str_max), c
        character(16) :: string
        integer :: ncol, i, j
        real(mp) :: numbers(str_max)
        logical :: lastisspace
        numbers = 0
        j = 1
        ncol = 0
        lastisspace=.true.
        do i=1,str_max
            read(*, '(a)', advance='no', iostat=ios) c
            if (ios /= 0) then
                write(string,*) buffer(j:i-1)
                !write(*,*) "ios="//str(ios)
                if (isspace(string)) then
                    ios = -3
                    !write(*,*) "new ios="//str(ios)
                else
                    read(string,*) numbers(ncol)
                    !write(*,*) numbers(ncol)
                end if
                exit
            end if
            buffer(i) = c
            if (lastisspace .and. .not. isspacesymbol(c)) then
                if (ncol /= 0) then
                    write(string,*) buffer(j:i-2)
                    read(string,*) numbers(ncol)
                    !write(*,*) numbers(ncol)
                end if
                ncol = ncol + 1
                j = i
            end if
            lastisspace = isspacesymbol(c)
        end do
        allocate(v(ncol))
        v = numbers(1:ncol)
    end subroutine

    subroutine input_real1D(text, v)
        character(*), intent(in) :: text
        real(mp), allocatable, intent(out) :: v(:)
        integer :: ios
        write(*,'(a$)') text
        call read_real1D(v, ios)
        call output_real1D('Получено', v)
    end subroutine

    subroutine read_real2D(v)
        real(mp), allocatable, intent(out) :: v(:,:)
        real(mp), allocatable :: w(:)
        real(mp), allocatable :: buffer(:,:)
        integer :: ios, sz_x, sz_y, i
        i = 0
        do
            i = i + 1
            call read_real1D(w, ios)
            if (ios == -3) exit
            !call output("Строка "//str(i)//" ", w)
            if (i == 1) then
                sz_x = size(w)                ! Определяем размер массива по первой строчке
                allocate(buffer(sz_x, y_max)) ! (ранее было по максимальной, но стало давать ошибку)
            end if                            ! Array bound mismatch for dimension 1 of array 'buffer' (100/4)
            buffer(:,i) = w(:)
            if (ios == -1) then
                i = i + 1
                exit
            end if
        end do
        sz_y = i - 1
        allocate(v(sz_x, sz_y))
        v = buffer(1:sz_x, 1:sz_y)
    end subroutine

    subroutine input_real2D(text, v)
        character(*), intent(in) :: text
        real(mp), allocatable, intent(out) :: v(:,:)
        write(*,'(a)') text
        call read_real2D(v)
        call output_real2D('Получено', v)
    end subroutine

end module