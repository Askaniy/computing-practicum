
! Модуль процедур вывода

module io_output
    use io_precision
    use io_general, only: tlen, str
    implicit none

    private
    public output

    interface output
        module procedure output_int0D, output_int1D, output_int2D, output_real0D, output_real1D, output_real2D, &
            output_complex0D, output_complex1D, output_complex2D, output_bool0D, output_bool1D
    end interface

    contains

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

    subroutine output_complex0D(text, a)
        character(*), intent(in) :: text
        complex(mp), intent(in) :: a
        write(*,'("'//text//'", 1x, f0.'//str(dp)//', "+", f0.'//str(dp)//', "i")') a
    end subroutine

    subroutine output_complex1D(text, a)
        character(*), intent(in) :: text
        complex(mp), intent(in) :: a(:)
        if (size(a) == 1) then
            call output_complex0D(text, a(1))
        else
            write(*,'("'//text//'", (1x, f0.'//str(dp)//', "+", f0.'//str(dp)//', "i")$)') a
            write(*,*)
        end if
    end subroutine

    subroutine output_complex2D(text, a)
        character(*), intent(in) :: text
        complex(mp), intent(in) :: a(:,:)
        integer :: sz_x, sz_y
        sz_x = size(a, dim=1)
        sz_y = size(a, dim=2)
        if (sz_x == 1) then
            call output_complex1D(text, reshape(a, [sz_y]))
        else if (sz_y == 1) then
            call output_complex1D(text, reshape(a, [sz_x]))
        else
            block
                integer :: l, l_ij, i, j, len_text
                len_text = tlen(text)
                l = 0
                do i = 1,sz_x
                    do j = 1,sz_y
                        l_ij = max(len(str(floor(real(a(i,j))))), len(str(floor(aimag(a(i,j))))))
                        if (l < l_ij) l = l_ij
                    end do
                end do
                l = l + 2 + dp
                if (len_text == 0) then
                    write(*,'('//str(sz_x)//'(f'//str(l)//'.'//str(dp)//',&
                    &                    "+", f'//str(l)//'.'//str(dp)//', "i")/,&
                    &        ('//str(sz_x)//'(f'//str(l)//'.'//str(dp)//',&
                    &                    "+", f'//str(l)//'.'//str(dp)//', "i")))') a
                else
                    write(*,'("'//text//'", '//str(sz_x)//'(f'//str(l)//'.'//str(dp)//',&
                    &                                  "+", f'//str(l)//'.'//str(dp)//', "i")/,&
                    &('//str(len_text)//'x, '//str(sz_x)//'(f'//str(l)//'.'//str(dp)//',&
                    &                                  "+", f'//str(l)//'.'//str(dp)//', "i")))') a
                end if
            end block
        end if
    end subroutine

    subroutine output_bool0D(text, a)
        character(*), intent(in) :: text
        logical, intent(in) :: a
        write(*,'("'//text//'", 1x, l)') a
    end subroutine

    subroutine output_bool1D(text, a)
        character(*), intent(in) :: text
        logical, intent(in) :: a(:)
        if (size(a) == 1) then
            call output_bool0D(text, a(1))
        else
            write(*,'("'//text//'", (1x, l)$)') a
            write(*,*)
        end if
    end subroutine

end module