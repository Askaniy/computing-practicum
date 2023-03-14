module game_of_life
    use omp_lib
    implicit none

    contains

    subroutine test()
        !$omp parallel 
        write(*,'("Hi from ", i0)') omp_get_thread_num()
        !$omp end parallel 
    end subroutine

end module
