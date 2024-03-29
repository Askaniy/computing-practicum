! Задание 9: Задача Коши для системы обыкновенных дифференциальных уравнений

module settings
    use io_precision, only: mp
    use math_general, only: pi
    implicit none

    ! Шаг интегрирования
    real(mp), parameter :: h = 0.1

    ! Конец интервала интегрирования
    integer, parameter :: end = 10

    ! Порядок методов Адамса
    integer, parameter :: m = 4

    ! Параметры математического маятника
    real(mp), parameter, private :: g=9.8, l=2, mu=0.1

    ! Вектор начальных условий (отпускаем маятник под 60°)
    real(mp), dimension(2), parameter :: x0 = [pi/3, 0._mp]

    contains

    ! Уравнение колебаний математического маятника
    pure function pendulum(t, x) result(x_dot)
        real(mp), intent(in) :: t, x(:)
        real(mp) :: x_dot(size(x))
        x_dot(1) = x(2)
        x_dot(2) = -mu * x(2) - g/l * sin(x(1))
    end function

end module

program quest9v1
    use io_precision
    use io_general, only: str
    use math_ode
    use settings
    implicit none
    
    integer :: i, n, d
    real(mp) :: t(int(end/h)+1)
    real(mp) :: x(size(x0), size(t))

    n = size(t)  ! количество шагов
    d = size(x0) ! размерность системы

    ! Формирование равномерного массива времени
    do concurrent (i=1:n)
        t(i) = (i-1) * h
    end do

    x = ode_simple(pendulum, t, x0)
    open(1, file='sp.dat')
        do i=1,n
            write(1,'('//str(1+d)//'f9.'//str(dp)//')') t(i), x(:,i)
        end do
    close(1)

    x = ode_runge_kutta_4(pendulum, t, x0)
    open(1, file='rk.dat')
        do i=1,n
            write(1,'('//str(1+d)//'f9.'//str(dp)//')') t(i), x(:,i)
        end do
    close(1)

    x = ode_adams_extrap(pendulum, t, x0, m)
    open(1, file='ae.dat')
        do i=1,n
            write(1,'('//str(1+d)//'f9.'//str(dp)//')') t(i), x(:,i)
        end do
    close(1)

    x = ode_adams_interp(pendulum, t, x0, m)
    open(1, file='ai.dat')
        do i=1,n
            write(1,'('//str(1+d)//'f9.'//str(dp)//')') t(i), x(:,i)
        end do
    close(1)
    
end program