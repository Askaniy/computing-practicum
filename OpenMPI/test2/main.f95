program MPItest2
	use mpi
	use my_io
	use my_math
    implicit none
    
    integer(4) :: err, myid, nproc, band_num, b
    real(mp) :: arr(3), arrr(3,3)
    integer, dimension(mpi_status_size) :: status

    call mpi_init(err)
    call mpi_comm_size(mpi_comm_world, nproc, err)
    call mpi_comm_rank(mpi_comm_world, myid, err)
	band_num = nproc - 1

    arr = [0, 0, 0]

    if (myid == 0) then
        !write(*,*) 'myid 0'
        arr = [0, 0, 0]
        call output('0: arr =', arr)
        !call mpi_send(arr, size(arr), mpi_real4, 1, 666, mpi_comm_world, err)
        !call mpi_gather(SBuf, SCount, SType, RBuf, RCount, RType, Root, Comm, Err)
        do b=1,band_num
            call mpi_recv(arrr(:,b), size(arr), mpi_real4, b, 666, mpi_comm_world, status, err)
        end do
        call output('0: arrr =', arrr)
    else if (myid == 1) then
        !call mpi_recv(arr, size(arr), mpi_real4, 0, 666, mpi_comm_world, status, err)
        arr = [1, 1, 1]
        !call output(str(myid)//'/'//str(band_num)//': arr =', arr)
        call mpi_send(arr, size(arr), mpi_real4, 0, 666, mpi_comm_world, err)
    else if (myid == 2) then
        arr = [2, 2, 2]
        !call output(str(myid)//'/'//str(band_num)//': arr =', arr)
        call mpi_send(arr, size(arr), mpi_real4, 0, 666, mpi_comm_world, err)
    else if (myid == 3) then
        arr = [3, 3, 3]
        !call output(str(myid)//'/'//str(band_num)//': arr =', arr)
        call mpi_send(arr, size(arr), mpi_real4, 0, 666, mpi_comm_world, err)
    end if

    !write(*,*) 'Hi'

    call mpi_finalize(err)
end program