
! Модуль обработки строк, функционал близок к аналогам на языке Python
! Изначально вдохновлено https://github.com/certik/fortran-utils/blob/master/src/utils.f90

module io_strings
    use io_general, only: str, tlen
    implicit none

    private
    public isspace, isspacesymbol, lower, upper, zfill

    contains

    ! Возвращает .true., если вся строка из пробельных символов или пуста
    pure logical function isspace(string)
        character(*), intent(in) :: string
        integer :: i
        isspace = all([(isspacesymbol(string(i:i)), i=1,len(string))])
    end function

    ! Возвращает .true. на пробельные символы space (32) и tab (9)
    pure logical function isspacesymbol(char)
        character, intent(in) :: char
        isspacesymbol = (iachar(char) == 32 .or. iachar(char) == 9)
    end function

    ! Возвращает строчку строчными латинскими символами
    pure function lower(s) result(t)
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

    ! Возвращает строчку заглавными латинскими символами
    pure function upper(s) result(t)
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

    ! Расширяет нулями влево целое число
    pure function zfill(i, n) result(s)
        integer, intent(in) :: i, n
        character(max(n, tlen(i))) :: s
        write(s,'(i0.'//str(n)//')') i
    end function

end module