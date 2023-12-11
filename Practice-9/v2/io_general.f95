
! Модуль основных процедур ввода-вывода и преобразования переменных

module io_general
    use io_precision
    implicit none

    private
    public isfile, str, tlen, read_argument

    integer, parameter, public :: str_max = 100 ! максимальная длина строки для записи числа

    interface str
        module procedure str_int, str_real
    end interface

    interface tlen
        module procedure len_int, len_real, len_str
    end interface

    interface read_argument
        module procedure read_argument_str, read_argument_int
    end interface

    contains

    ! Проверка на существование файла
    logical function isfile(filename)
        character(*), intent(in) :: filename
        inquire(file=trim(filename), exist=isfile)
    end function


    ! Серия функций str. Принимает целые и вещественные переменные, возвращает строку
    ! Пример использования: s = str(2022)
    
    pure function str_int(i) result(s)
        integer, intent(in) :: i
        character(len_int(i)) :: s
        write(s,'(i0)') i
    end function
    
    pure function str_real(r) result(s)
        real(mp), intent(in) :: r
        character(len_real(r, '(f0.'//str_int(dp)//')')) :: s
        write(s,'(f0.'//str_int(dp)//')') r
    end function


    ! Серия функций tlen. Модификация len для работы с числами и UTF-8
    ! Пример использования: l = tlen('Привет, мир!')

    pure integer function len_int(i) result(sz)
        integer, intent(in) :: i
        character(str_max) :: s
        write(s,'(i0)') i
        sz = len_trim(s)
    end function
    
    pure integer function len_real(r, fmt) result(sz)
        real(mp), intent(in) :: r
        character(*), intent(in) :: fmt
        character(str_max) :: s
        write(s, fmt) r
        sz = len_trim(s)
    end function

    pure function len_str(string) result(sz)
        character(*), intent(in) :: string
        integer :: i, sz
        ! считает непечатные байты за половину символа
        sz = len(string) * 2
        do i = 1,len(string)
            if (ichar(string(i:i)) > 127) sz = sz - 1
        end do
        sz = sz / 2
    end function


    ! Серия подпрограмм read_argument. Записывает аргумент вызова под заданным номером
    ! в указанную переменную (например, неразмещённую строку)

    subroutine read_argument_str(index, arg, default)
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

    subroutine read_argument_int(index, int, default)
        character(:), allocatable :: arg
        integer, intent(in) :: index
        integer :: int
        integer, optional :: default
        call read_argument_str(index, arg, str(default))
        read(arg,*) int
    end subroutine

end module