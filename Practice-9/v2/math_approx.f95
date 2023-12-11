
! Модуль аппроксимации данных

module math_approx
    use io_precision, only: mp
    use math_general, only: pi, diffs, find_index, multiply
    use math_sle, only: solve_pentadiagdominant_sle
    implicit none

    private
    public polynomial_interp, spline_approx
    
    integer, private :: i, j, k

    contains

    ! Интерполирование полиномами
    function polynomial_interp(grid, q, a, b) result(interpolated)
        integer, intent(in) :: q ! число разбиений интервала
        integer :: n, m
        real(mp), intent(in) :: grid(:, 0:), a, b ! начальная сетка в виде колонок x, f(x)
        real(mp) :: interpolated(2, 0:(size(grid, dim=2)-1)*q) ! результат в том же виде
        n = size(grid, dim=2) - 1
        m = size(interpolated, dim=2) - 1 ! число интервалов
        interpolated(1,:) = [(a + j*(b-a)/m, j=0,m)]
        interpolated(2,:) = [(dot_product( & ! интерполяционный полином
                                grid(2,:), [(product( & ! интерполяционный базис
                                    (interpolated(1,j) - grid(1,:)) / (grid(1,k) - grid(1,:)), &
                                    mask = grid(1,k)/=grid(1,:)), k=0,n)]), &
                            j=0,m)]
    end function

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
                array(1, :) = [(cos((2.0_mp*k + 1) / (2.0_mp*n + 2) * pi), k=n,0,-1)]
            end if
        close(1)
    end subroutine

    ! Аппроксимация сплайнами
    function spline_approx(XYP, q) result(approximated)
        integer, intent(in) :: q ! число разбиений интервала
        integer :: n, m
        real(mp), intent(in) :: XYP(:, 0:) ! начальная сетка в виде колонок X, Y, P
        real(mp) :: approximated(2, 0:(size(XYP, dim=2)-1)*q), & ! результат в виде X, Y
                    s(0:size(XYP, dim=2)-1), r(0:size(XYP, dim=2)-1), & ! промежуточные вектора
                    h(0:size(XYP, dim=2)-2), h_j, xi, t
        n = size(XYP, dim=2) - 1 ! число интервалов входной сетки
        call spline_vectors(XYP, n, s, r)
        m = n * q ! число интервалов новой сетки
        h_j = (XYP(1,n) - XYP(1,0)) / m ! шаг новой сетки
        h = diffs(XYP(1,:)) ! предвычисленный массив разностей иксов
        do concurrent (j=0:m) ! можно было итерировать по входной сетке, но так должно быть быстрее
            xi = XYP(1,0) + h_j * j ! текущая точка
            i = find_index(XYP(1,:), xi) - 1 ! сдвиг из-за местного отсчёта от нуля
            t = (xi - XYP(1,i)) / h(i)
            approximated(1,j) = xi
            approximated(2,j) = r(i)*(1-t) + r(i+1)*t - h(i)*h(i)*t*(1-t)*((2-t)*s(i)+(1+t)*s(i+1))/6
        end do
    end function

    ! Вспомогательная функция, формирующая вектора R и S
    subroutine spline_vectors(XYP, n, s, r)
        integer, intent(in) :: n ! передаю внутрь для упрощения инициализации размеров ниже
        real(mp), intent(in) :: XYP(1:3, 0:n)
        real(mp), intent(inout) :: s(n+1), r(n+1)
        real(mp) :: a(-1:1, n+1), b(-1:1, n+1), qbt(-1:1, n+1), aa(-2:2, n+1) ! трёхдиагональные и пятидиагональные
        a = 0
        a(0,1) = 2*XYP(1,1) - 2*XYP(1,0) ! напоминание, что здесь "XYP(1,:)" - это колонка иксов
        a(0,2) = 2*XYP(1,2) - 2*XYP(1,0) ! 2 за скобку не выношу, т.к. "вычитать почти равные числа - плохо"
        a(1,2) = XYP(1,2) - XYP(1,1)
        a(-1,n) = XYP(1,n-1) - XYP(1,n-2)
        a(0,n) = 2*XYP(1,n) - 2*XYP(1,n-2)
        a(0,n+1) = 2*XYP(1,n) - 2*XYP(1,n-1)
        b = 0
        b(-1,2) = 1/(XYP(1,1) - XYP(1,0))  ! предрасчитываю вручную первые и последние две строки, а не расширяю нулями, т.к.
        b(1,2) = 1/a(1,2)                  ! 1) эти матрицы потом перемножаются, менять их размеры не удобно
        b(0,2) = -b(-1,2) - b(1,2)         ! 2) для матрицы A другого варианта нет
        b(-1,n) = 1/a(-1,n)
        b(1,n) = 1/(XYP(1,n) - XYP(1,n-1))
        b(0,n) = -b(-1,n) - b(1,n)
        do concurrent (i=3:n-1) ! быстрее за счёт распараллеливания, но больше вычислений, чем если итеративно
            a(-1,i) = XYP(1,i-1) - XYP(1,i-2)
            a(1, i) = XYP(1,i) - XYP(1,i-1)
            a(0, i) = 2*XYP(1,i) - 2*XYP(1,i-2)
            b(-1,i) = 1/a(-1,i)
            b(1, i) = 1/a(1,i)
            b(0, i) = -b(-1,i) - b(1,i)
        end do
        qbt = 0
        do concurrent (i=2:n) ! одновременное транспонирование B и умножение на Q: тогда будет 3-диг. матрица, а не 5-диг.
            qbt(-1,i+1) = b(-1,i) / XYP(3,i)
            qbt(0,i) = b(0,i) / XYP(3,i-1)
            qbt(1,i-1) = b(1,i) / XYP(3,i-2)
        end do
        aa = 6 * multiply(b, qbt, 'tridiagonal')
        aa(-1:1,:) = aa(-1:1,:) + a ! правая часть СЛУ
        !call output('aa = ', aa) ! слегка несимметрична?
        s = solve_pentadiagdominant_sle(aa, 6 * multiply(b, XYP(2,:), 'tridiagonal'))
        r = XYP(2,:) - multiply(qbt, s, 'tridiagonal') ! вектор результатов R = Y - Q B^T S
    end subroutine

end module