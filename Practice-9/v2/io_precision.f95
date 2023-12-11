
! Модуль объявления точности вычислений

module io_precision
    implicit none
    
    integer, parameter :: mp = 8 ! "my precision", число байт для типа real (для int 4 байта всегда)
    integer, parameter :: dp = 4 ! "decimal places", число знаков после запятой в форматированном выводе

end module