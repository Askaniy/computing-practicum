module my_consts
    implicit none

    integer, parameter :: mp = 4 ! "my precision", число байт для типа real (для int 4 байта всегда)
    integer, parameter :: dp = 4 ! "decimal places", число знаков после запятой в форматированном выводе 
    real(mp), parameter :: PI = 4*atan(1.0_mp)

end module