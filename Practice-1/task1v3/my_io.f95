module my_io
    implicit none

    integer, parameter :: mp = 4 ! "my precision", число байт для типа real (для int всегда 4 байта)
    integer, parameter :: dp = 3 ! "decimal places", число знаков после запятой в форматированном выводе
    
    interface str
        module procedure str_int, str_real
    end interface

    interface input
        module procedure input_char, input_int, input_real, &
            input_int1D, input_int2D, input_real1D, input_real2D
    end interface

    interface output
        module procedure output_int1D, output_int2D, output_real1D, output_real2D
    end interface

    contains


    ! Серия функций, требуемых для вычислительного практикума

    function import_matrix(path, mode) result(array)
        integer :: i, j, n
        real(mp), allocatable :: array(:,:)
        character(*) :: path, mode
        open(1, file=path, status='old')
            read(1,'(2x, i5)') n ! размер матрицы задаётся с третьего символа первой строки
            if (mode == 'square') then
                allocate(array(n, n))
                do j = 1,n
                    read(1,*) (array(i, j), i=1,n)
                end do
            elseif (mode == 'tridiagonal') then
                allocate(array(3, n))
                array = 0
                read(1,*) (array(i, 1), i=2,3) ! первые два элемента со сдвигом
                do j = 2,n-1
                    read(1,*) (array(i, j), i=1,3)
                end do
                read(1,*) (array(i, n), i=1,2) ! последние два тоже
            end if
        close(1)
    end function


    ! Серия функций str. Принимает целые и вещественные переменные, возвращает строку
    ! Пример использования: s = str(2022)

    pure integer function str_int_len(i) result(sz)
        integer, intent(in) :: i
        integer, parameter :: MAX_STR = 100
        character(MAX_STR) :: s
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
        integer, parameter :: MAX_STR = 100
        character(MAX_STR) :: s
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
            write(*,*) "Ошибка при чтении строки. Индекс ошибки "//str(ios)
            stop 1
        endif
        write(*,*) "Received '"//v//"'"
    end subroutine input_char

    subroutine input_int(text, v)
        character(*), intent(in) :: text
        integer, intent(out) :: v
        integer :: ios
        write(*,'(a$)') text
        read(*,*,iostat=ios) v
        if (ios > 0) then
            write(*,*) "Ошибка при чтении целого числа. Индекс ошибки "//str(ios)
            stop 1
        endif
        write(*,'("Received ", i0)') v
    end subroutine

    subroutine input_real(text, v)
        character(*), intent(in) :: text
        real(mp), intent(out) :: v
        integer :: ios
        write(*,'(a$)') text
        read(*,*,iostat=ios) v
        if (ios > 0) then
            write(*,*) "Ошибка при чтении вещ. числа. Индекс ошибки "//str(ios)
            stop 1
        endif
        write(*,*) "Received", v
    end subroutine

    subroutine read_int1D(v, ios)
        integer, allocatable, intent(out) :: v(:)
        integer, intent(out) :: ios
        integer, parameter :: nchar=100 ! максимальное количество символов в строке
        character :: buffer(nchar), c
        character(16) :: string
        integer :: ncol, i, j, numbers(nchar)
        logical :: lastisspace
        numbers = 0; j = 1; ncol = 0; lastisspace=.true.
        do i=1,nchar
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
        call output_int1D("Received ", v)
    end subroutine

    subroutine read_int2D(v)
        integer, allocatable, intent(out) :: v(:,:)
        integer, allocatable :: w(:)
        integer, parameter :: y_max=100 ! максимальное количество строк
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
        call output_int2D("Received ", v)
    end subroutine

    subroutine read_real1D(v, ios)
        real(mp), allocatable, intent(out) :: v(:)
        integer, intent(out) :: ios
        integer, parameter :: nchar=100
        character :: buffer(nchar), c
        character(16) :: string
        integer :: ncol, i, j
        real(mp) :: numbers(nchar)
        logical :: lastisspace
        numbers = 0; j = 1; ncol = 0; lastisspace=.true.
        do i=1,nchar
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
        call output_real1D("Received ", v)
    end subroutine

    subroutine read_real2D(v)
        real(mp), allocatable, intent(out) :: v(:,:)
        real(mp), allocatable :: w(:)
        integer, parameter :: y_max=100 ! максимальное количество строк
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
        call output_real2D("Received ", v)
    end subroutine


    ! Серия подпрограмм output. Реализует печать 1D/2D массивов и целыми и вещественными элементами
    ! Не требует форматирования - использует информацию в массиве для определения размера
    !                                               и переменную dp для количества знаков после запятой
    ! Отступ, соразмерный тексту, корректно работает только для латиницы
    ! Пример использования: call output("array = ", a)

    subroutine output_int1D(text, a)
        character(*), intent(in) :: text
        integer, intent(in) :: a(:)
        write(*,'("'//text//'", (1x, i0)$)') a
        write(*,*)
    end subroutine

    subroutine output_int2D(text, a)
        character(*), intent(in) :: text
        integer, intent(in) :: a(:,:)
        integer :: l, i, j, sz_x, len_text=1
        if (len(text) > 0) len_text = len(text)
        sz_x = size(a, dim=1)
        l = 0
        do i = 1,sz_x
            do j = 1,size(a,2)
                if (l < len(str(a(i,j)))) l = len(str(a(i,j)))
            end do
        end do
        l = l + 1
        write(*,'("'//text//'", '//str(sz_x)//'i'//str(l)//'/,&
        &('//str(len_text)//'x, '//str(sz_x)//'i'//str(l)//'))') a
    end subroutine

    subroutine output_real1D(text, a)
        character(*), intent(in) :: text
        real(mp), intent(in) :: a(:)
        write(*,'("'//text//'", (1x, f0.'//str(dp)//')$)') a
        write(*,*)
    end subroutine

    subroutine output_real2D(text, a)
        character(*), intent(in) :: text
        real(mp), intent(in) :: a(:,:)
        integer :: l, i, j, sz_x, len_text=1
        if (len(text) > 0) len_text = len(text)
        sz_x = size(a, dim=1)
        l = 0
        do i = 1,sz_x
            do j = 1,size(a,2)
                if (l < len(str(int(a(i,j))))) l = len(str(int(a(i,j))))
            end do
        end do
        l = l + 2 + dp
        write(*,'("'//text//'", '//str(sz_x)//'f'//str(l)//'.'//str(dp)//'/,&
        &('//str(len(text))//'x, '//str(sz_x)//'f'//str(l)//'.'//str(dp)//'))') a
    end subroutine


    ! Серия функций обработки строк, функционал близок к аналогам на языке Python
    ! Источник: https://github.com/certik/fortran-utils/blob/master/src/utils.f90

    logical function isspacesymbol(char) result(res) ! возвращает .true. на пробельные символы space (32) и tab (9)
        character, intent(in) :: char
        if (iachar(char) == 32 .or. iachar(char) == 9) then
            res = .true.
        else
            res = .false.
        end if
    end function
    
    logical function isspace(string) result(res) ! возвращает .true. если вся строка из пробельных символов
        character(*), intent(in) :: string
        integer :: i
        do i = 1, len(string)
            if (.not. isspacesymbol(string(i:i))) exit
        end do
        res = (i>len(string))
    end function

    function lower(s) result(t) ! Возвращает строчку строчными латинскими символами
        character(*), intent(in) :: s
        character(len(s)) :: t
        integer :: i, diff
        t = s; diff = ichar('A')-ichar('a')
        do i = 1,len(t)
            if (ichar(t(i:i)) >= ichar('A') .and. ichar(t(i:i)) <= ichar('Z')) then
                t(i:i) = char(ichar(t(i:i)) - diff)
            end if
        end do
    end function

    function upper(s) result(t) ! Возвращает строчку заглавными латинскими символами
        character(*), intent(in) :: s
        character(len(s)) :: t
        integer :: i, diff
        t = s; diff = ichar('A')-ichar('a')
        do i = 1,len(t)
            if (ichar(t(i:i)) >= ichar('a') .and. ichar(t(i:i)) <= ichar('z')) then
                t(i:i) = char(ichar(t(i:i)) + diff)
            end if
        end do
    end function

end module my_io