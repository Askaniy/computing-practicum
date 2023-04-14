program MPItest
    implicit none

    integer :: n, i
    real(4), allocatable :: qh2(:,:)

    open(1, file='sources.dat', status='old')
        read(1,'(2x, i5)') n
        allocate(qh2(n,n))
        read(1,*) qh2
    close(1)

    write(*,*) qh2
    
end program