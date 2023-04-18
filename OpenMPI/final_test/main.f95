program MPItest
	use mpi
	use my_io
	use my_math
    implicit none
    
    integer(4) :: err, rank, mpiSize

    integer :: n, i, j, b, band_num, band_len, step
	integer, parameter :: k=10, max_step=100 ! шаг итераций при записи в файл и максимальное количество шагов
    real(mp), allocatable :: qh2(:,:), qh2_crop(:,:), u0(:,:), u0_crop(:,:), u1(:,:), u1_crop(:,:)
    
    call mpi_init(err)
    
    call mpi_comm_size(mpi_comm_world, mpiSize, err)
    call mpi_comm_rank(mpi_comm_world, rank, err)
	band_num = mpiSize - 1
    write(*,*) str(rank)//'/'//str(band_num)

	if (rank == 0) then ! чтение в координаторе и нулевая итерация
		open(1, file='sources.dat', status='old')
		    read(1,'(2x, i5)') n
		    allocate(qh2(n, n))
		    allocate(u0(0:n+1, 0:n+1))
		    allocate(u1(0:n+1, 0:n+1))
			qh2 = 0
		    read(1,*) qh2(1:n, 1:n)
			band_len = n / band_num
		close(1)
		u0 = 0
		call save_u(u0, 0)
		u1 = 0
	end if

	if (rank == 0) then
		! *отправляем qh2*
		! *отправляем u0_crop*
		! *отправляем u1_crop*
	else
		allocate(qh2_crop(band_len, n))
		! *получаем и записываем поле qh2_crop*
		allocate(u0_crop(0:band_len+1, 0:n+1))
		u0_crop = 0
		! *получаем и записываем поле u0_crop*
		allocate(u1_crop(0:band_len+1, 0:n+1))
		u1_crop = 0
		! *получаем и записываем поле u1_crop*
	end if

	do step=1,max_step
		if (rank == 0) then ! координатор
			if (mod(step, k) == 0) then
				! *собираем поля в u1*
				call save_u(u1, step)
			end if
		else
			! *получаем и записываем границы*
			if (rank == 1) then
				! получаем только от rank=2
			else if (rank == band_num)
				! получаем только от rank=band_num-1
			else
				! получаем от rank-1 и rank+1
			end if

			forall (i=1:band_len, j=1:n)
				u1_crop(i, j) = (u0_crop(i+1, j) + u0_crop(i-1, j) + u0_crop(i, j+1) + u0_crop(i, j-1) + qh2_crop(i, j)) / 4
			end forall

			if (mod(step, k) == 0) then
				! *отправляем поле*
			end if

			if (step /= max_step) then
				! *отправляем границы соседним полям*
				if (rank == 1) then
					! отправляем только rank=2
				else if (rank == band_num)
					! отправляем только rank=band_num-1
				else
					! отправляем rank-1 и rank+1
				end if
			end if

			u0_crop = u1_crop
		end if
	end do

    call mpi_finalize(err)

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
