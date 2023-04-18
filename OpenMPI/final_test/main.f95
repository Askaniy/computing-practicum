program MPItest
	use mpi
	use my_io
	use my_math
    implicit none
    
    integer(4) :: Err, Rank, mpiSize

    integer :: n, i, j, k, band_num, band_len
    real(mp), allocatable :: qh2(:,:), u0(:,:), u1(:,:)
    
    call mpi_init(Err)
    
    call mpi_comm_size(mpi_comm_world, mpiSize, Err)
    call mpi_comm_rank(mpi_comm_world, Rank, Err)
	band_num = mpiSize - 1
    write(*,*) str(Rank)//'/'//str(band_num)

	if (rank == 0) then
		
		open(1, file='sources.dat', status='old')
		    read(1,'(2x, i5)') n
		    allocate(qh2(n, n))
		    allocate(u0(0:n+1, 0:n+1))
		    allocate(u1(0:n+1, 0:n+1))
			qh2 = 0
		    read(1,*) qh2(1:n, 1:n)
			band_len = n / band_num
		close(1)
		call output('qh2 =', qh2)

		! Нулевая итерация
		u0 = 0
		call save_u(u0, 0)

		do k=1,band_num
			forall (i=(k-1)*band_len+1:k*band_len, j=1:n)
				u1(i, j) = (u0(i+1, j) + u0(i-1, j) + u0(i, j+1) + u0(i, j-1) + qh2(i, j)) / 4
			end forall
		end do
		call save_u(u1, 1)

	end if

    call mpi_finalize(Err)

	contains

	subroutine save_u(u, indx)
        real(mp), intent(in) :: u(0:,0:)
		integer :: m, indx, row

		m = size(u, dim=2)
		open(1, file='data'//str(indx)//'.dat')
			write(1,'("# ", i0)') m-2
			do row = 1,m-1
				write(1,'('//str(m)//'(e10.'//str(dp)//'))') u(:, row)
			end do
		close(1)
	end subroutine

end program
