program MPItest2
	use mpi
	use my_io
	use my_math
    implicit none
    
    integer(4) :: err, myid, nproc, band_num
    real(mp) :: arr(3)
    integer, dimension(mpi_status_size) :: status

    call mpi_init(err)
    call mpi_comm_size(mpi_comm_world, nproc, err)
    call mpi_comm_rank(mpi_comm_world, myid, err)
	band_num = nproc - 1

    arr = [0, 0, 0]

    if (myid == 0) then
        !write(*,*) 'myid 0'
        arr = [1, 2, 3]
        call output('0: arr =', arr)
        call mpi_send(arr, size(arr), mpi_real4, 1, 666, mpi_comm_world, err)
    else if (myid == 1) then
        call mpi_recv(arr, size(arr), mpi_real4, 0, 666, mpi_comm_world, status, err)
        call output(str(myid)//'/'//str(band_num)//': arr =', arr)
    end if

    !write(*,*) 'Hi'

    call mpi_finalize(err)
end program