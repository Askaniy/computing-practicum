program MPItest
	use my_io
	use my_math
	use mpi
    implicit none
    
    integer(4) :: Err, Rank, Size

    integer :: n, i
    real(4), allocatable :: qh2(:,:)
    
    call mpi_init(Err)
    
    call mpi_comm_size(mpi_comm_world, Size, Err)
    call mpi_comm_rank(mpi_comm_world, Rank, Err)
    write(*,*) str(Rank)//'/'//str(Size-1)

	if (rank == 0) then
		open(1, file='sources.dat', status='old')
		    read(1,'(2x, i5)') n
		    allocate(qh2(n,n))
		    read(1,*) qh2
		close(1)
		call output('qh2 =', qh2)
	end if

    call mpi_finalize(Err)
end program
