program MPItest
	use mpi
	use my_io
	use my_math
    implicit none
    
    integer(4) :: Err, Rank, mpiSize

    integer :: n, i, j, b, band_num, band_len, step
	integer, parameter :: k=10, max_step=100 ! шаг итераций записи в файл и максимальное количество шагов
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

		! Нулевая итерация
		u0 = 0
		call save_u(u0, 0)

		! Первая и далее
		u1 = 0
		do step=1,max_step
			do b=1,band_num
				forall (i=(b-1)*band_len+1:b*band_len, j=1:n)
					u1(i, j) = (u0(i+1, j) + u0(i-1, j) + u0(i, j+1) + u0(i, j-1) + qh2(i, j)) / 4
				end forall
			end do
			if (mod(step, k) == 0) then
				call save_u(u1, step)
				!call output('u'//str(step)//' =', u1)
			end if
			u0 = u1
		end do

	end if

    call mpi_finalize(Err)

	contains

	subroutine save_u(u, indx)
        real(mp), intent(in) :: u(:,:)
		integer :: m, indx, row
		m = size(u, dim=2)
		open(1, file='data/data'//str(indx)//'.dat')
			write(1,'("# ", i0)') m-2
			do row = 2,m-1
				write(1,'('//str(m)//'(f10.'//str(dp)//'))') u(2:m-1, row)
			end do
		close(1)
	end subroutine

end program
