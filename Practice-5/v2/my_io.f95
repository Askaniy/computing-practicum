module my_io
    implicit none

    private
    public str, input, output, isspacesymbol, isspace, lower, upper, &
        swap, read_argument, import_grid, import_vector, import_matrix, tlen

    integer, parameter, public :: mp = 4 ! "my precision", число байт для типа real (для int всегда 4 байта)
    integer, parameter, public :: dp = 3 ! "decimal places", число знаков после запятой в форматированном выводе

    integer, parameter :: str_max = 100 ! максимальная длина строки, конвертируемой в int или real
    integer, parameter :: y_max = 100 ! максимальное количество строк в импортируемой 2D матрице

    real(mp), parameter, public :: PI = 4*atan(1.0_mp)

    interface swap
        module procedure swap_int, swap_real
    end interface
    
    interface str
        module procedure str_int, str_real
    end interface

    interface input
        module procedure input_char, input_int, input_real, input_int1D, input_int2D, input_real1D, input_real2D
    end interface

    interface output
        module procedure output_int0D, output_int1D, output_int2D, output_real0D, output_real1D, output_real2D
    end interface

    contains


    ! Серия функций, потребовавшихся для вычислительного практикума

    ! Читает файл, заполняет колонку значений, границы аргументов и считает отмасштабированные значения
    subroutine import_grid(path, array, a, b, mode)
        integer :: k, n
        real(mp) :: a, b
        real(mp), allocatable :: array(:,:)
        character(*) :: path, mode
        open(1, file=path, status='old')
            read(1,'(2x, i5)') n
            allocate(array(2, 0:n)) ! точек на одну больше, чем интервалов
            read(1,*) a, b, array(2, :) ! чтение границ и столбца игреков (второго)
            if (mode == 'uniform') then
                array(1, :) = [(2.0_mp*k/n - 1, k=0,n)] ! вычисление столбца иксов (первого)
            elseif (mode == 'chebyshev') then
                array(1, :) = [(cos((2.0_mp*k + 1) / (2.0_mp*n + 2) * PI), k=n,0,-1)]
            end if
        close(1)
    end subroutine

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
            if (trim(mode) == 'square') then
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

    ! Записывает аргумент вызова под заданным номером в неразмещённую строку
    subroutine read_argument(index, arg, default)
        character(:), allocatable :: arg
        integer, intent(in) :: index
        integer :: arg_len, ios
        character(*), optional :: default
        call get_command_argument(number=index, length=arg_len)
        allocate(character(arg_len) :: arg)
        call get_command_argument(number=index, value=arg, status=ios)
        if (ios > 0) then
            write(*,*) 'Не удалось прочесть аргумент '//str(index)//'. Индекс ошибки '//str(ios)
            if (present(default)) arg = default
        endif
        write(*,*) 'Получено "'//arg//'"'
    end subroutine


    ! Серия подпрограмм swap. Переставляет что угодно
    ! Пример использования: call swap(matrix(j,:), matrix(k,:))

    elemental subroutine swap_int(a, b)
        integer, intent(inout) :: a, b
        integer :: tmp
        tmp = a
        a = b
        b = tmp
    end subroutine

    elemental subroutine swap_real(a, b)
        real(mp), intent(inout) :: a, b
        real(mp) :: tmp
        tmp = a
        a = b
        b = tmp
    end subroutine


    ! Серия функций str. Принимает целые и вещественные переменные, возвращает строку
    ! Пример использования: s = str(2022)

    pure integer function str_int_len(i) result(sz)
        integer, intent(in) :: i
        character(str_max) :: s
        write(s,'(i0)') i
        sz = len_trim(s)
    end function
    
    pure function str_int(i) result(s)
        integer, intent(in) :: i
        character(str_int_len(i)) :: s
        write(s,'(i0)') i
    end function
    
    pure integer function str_real_len(r, fmt) result(sz)
        real(mp), intent(in) :: r
        character(*), intent(in) :: fmt
        character(str_max) :: s
        write(s, fmt) r
        sz = len_trim(s)
    end function
    
    pure function str_real(r) result(s)
        real(mp), intent(in) :: r
        character(str_real_len(r, '(f0.'//str_int(dp)//')')) :: s
        write(s,'(f0.'//str_int(dp)//')') r
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


    ! Серия подпрограмм output. Реализует печать 1D/2D массивов и целыми и вещественными элементами
    ! Не требует форматирования - использует информацию массива для определения размера
    !                                               и переменную dp для количества знаков после запятой
    ! Пример использования: call output('array =', a)

    subroutine output_int0D(text, a)
        character(*), intent(in) :: text
        integer, intent(in) :: a
        write(*,'("'//text//'", 1x, i0)') a
    end subroutine

    subroutine output_int1D(text, a)
        character(*), intent(in) :: text
        integer, intent(in) :: a(:)
        if (size(a) == 1) then
            call output_int0D(text, a(1))
        else
            write(*,'("'//text//'", (1x, i0)$)') a
            write(*,*)
        end if
    end subroutine

    subroutine output_int2D(text, a)
        character(*), intent(in) :: text
        integer, intent(in) :: a(:,:)
        integer :: sz_x, sz_y
        sz_x = size(a, dim=1)
        sz_y = size(a, dim=2)
        if (sz_x == 1) then
            call output_int1D(text, reshape(a, [sz_y]))
        else if (sz_y == 1) then
            call output_int1D(text, reshape(a, [sz_x]))
        else
            block
                integer :: l, l_ij, i, j, len_text
                len_text = tlen(text)
                l = 0
                do i = 1,sz_x
                    do j = 1,size(a, dim=2)
                        l_ij = len(str(a(i,j)))
                        if (l < l_ij) l = l_ij
                    end do
                end do
                l = l + 1
                if (len_text == 0) then
                    write(*,'('//str(sz_x)//'i'//str(l)//'/,&
                            &('//str(sz_x)//'i'//str(l)//'))') a
                else
                    write(*,'("'//text//'", '//str(sz_x)//'i'//str(l)//'/,&
                    &('//str(len_text)//'x, '//str(sz_x)//'i'//str(l)//'))') a
                end if
            end block
        end if
    end subroutine

    subroutine output_real0D(text, a)
        character(*), intent(in) :: text
        real(mp), intent(in) :: a
        write(*,'("'//text//'", 1x, f0.'//str(dp)//')') a
    end subroutine

    subroutine output_real1D(text, a)
        character(*), intent(in) :: text
        real(mp), intent(in) :: a(:)
        if (size(a) == 1) then
            call output_real0D(text, a(1))
        else
            write(*,'("'//text//'", (1x, f0.'//str(dp)//')$)') a
            write(*,*)
        end if
    end subroutine

    subroutine output_real2D(text, a)
        character(*), intent(in) :: text
        real(mp), intent(in) :: a(:,:)
        integer :: sz_x, sz_y
        sz_x = size(a, dim=1)
        sz_y = size(a, dim=2)
        if (sz_x == 1) then
            call output_real1D(text, reshape(a, [sz_y]))
        else if (sz_y == 1) then
            call output_real1D(text, reshape(a, [sz_x]))
        else
            block
                integer :: l, l_ij, i, j, len_text
                len_text = tlen(text)
                l = 0
                do i = 1,sz_x
                    do j = 1,sz_y
                        l_ij = len(str(floor(a(i,j))))
                        if (l < l_ij) l = l_ij
                    end do
                end do
                l = l + 2 + dp
                if (len_text == 0) then
                    write(*,'('//str(sz_x)//'f'//str(l)//'.'//str(dp)//'/,&
                            &('//str(sz_x)//'f'//str(l)//'.'//str(dp)//'))') a
                else
                    write(*,'("'//text//'", '//str(sz_x)//'f'//str(l)//'.'//str(dp)//'/,&
                    &('//str(len_text)//'x, '//str(sz_x)//'f'//str(l)//'.'//str(dp)//'))') a
                end if
            end block
        end if
    end subroutine


    ! Серия функций обработки строк, функционал близок к аналогам на языке Python
    ! Вдохновлено https://github.com/certik/fortran-utils/blob/master/src/utils.f90

    pure logical function isspacesymbol(char) ! возвращает .true. на пробельные символы space (32) и tab (9)
        character, intent(in) :: char
        isspacesymbol = (iachar(char) == 32 .or. iachar(char) == 9)
    end function
    
    pure logical function isspace(string) ! возвращает .true., если вся строка из пробельных символов или пуста
        character(*), intent(in) :: string
        integer :: i
        isspace = all([(isspacesymbol(string(i:i)), i=1,len(string))])
    end function

    pure function tlen(string) result(sz) ! возвращает длину строки, считая непечатные байты за половину символа
        character(*), intent(in) :: string
        integer :: i, sz
        sz = len(string) * 2
        do i = 1,len(string)
            if (ichar(string(i:i)) > 127) sz = sz - 1
        end do
        sz = sz / 2
    end function

    pure function lower(s) result(t) ! Возвращает строчку строчными латинскими символами
        character(*), intent(in) :: s
        character(len(s)) :: t
        integer :: i, diff
        t = s
        diff = ichar('A') - ichar('a')
        do i = 1,len(t)
            if (ichar(t(i:i)) >= ichar('A') .and. ichar(t(i:i)) <= ichar('Z')) then
                t(i:i) = char(ichar(t(i:i)) - diff)
            end if
        end do
    end function

    pure function upper(s) result(t) ! Возвращает строчку заглавными латинскими символами
        character(*), intent(in) :: s
        character(len(s)) :: t
        integer :: i, diff
        t = s
        diff = ichar('A') - ichar('a')
        do i = 1,len(t)
            if (ichar(t(i:i)) >= ichar('a') .and. ichar(t(i:i)) <= ichar('z')) then
                t(i:i) = char(ichar(t(i:i)) + diff)
            end if
        end do
    end function

end module