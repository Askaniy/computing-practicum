! Задание 9: Задача Коши для системы обыкновенных дифференциальных уравнений

module settings
    use my_consts
    implicit none

    ! Конец интервала интегрирования
    integer, parameter :: end = 100

    ! Вектор начальных условий (отпускаем маятник под 60°)
    real(mp), dimension(2), parameter :: x0 = [pi/3, 0.]

    ! Шаг интегрирования
    real(mp), parameter :: h = 0.1

    ! Параметры математического маятника
    real(mp), parameter, private :: g=9.8, l=2, mu=0.1

    ! Порядок методов Адамса?

    contains

    ! Уравнение колебаний математического маятника
    function pendulum(t, x) result(x_dot)
        real(mp), intent(in) :: t, x(2)
        real(mp), dimension(2) :: x_dot
        x_dot(1) = x(2)
        x_dot(2) = -mu * x(2) - g/l * sin(x(1))
    end function

end module

program quest9v1
    use settings
    use my_io
    use my_math
    use my_consts
    implicit none
    
    integer :: i, n, d
    real(mp), dimension(end+1) :: t=[(i*h, i=0,end)]
    real(mp) :: x(size(x0), size(t))

    n = size(t)  ! количество шагов
    d = size(x0) ! размерность системы

    x = ode_simple(t, x0)
    open(1, file='sp.dat')
        do i=1,n
            write(1,'('//str(1+d)//'f9.'//str(dp)//')') t(i), x(:,i)
        end do
    close(1)

    x = ode_runge_kutta_4(t, x0)
    open(1, file='rk.dat')
        do i=1,n
            write(1,'('//str(1+d)//'f9.'//str(dp)//')') t(i), x(:,i)
        end do
    close(1)

    x = ode_adams_extrap(t, x0)
    open(1, file='ae.dat')
        do i=1,n
            write(1,'('//str(1+d)//'f9.'//str(dp)//')') t(i), x(:,i)
        end do
    close(1)

    x = ode_adams_interp(t, x0)
    open(1, file='ai.dat')
        do i=1,n
            write(1,'('//str(1+d)//'f9.'//str(dp)//')') t(i), x(:,i)
        end do
    close(1)
    
end program