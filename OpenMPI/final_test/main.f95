program MPIfinaltest
	use mpi
	use my_io
	use my_math
    implicit none
    
    integer(4) :: err, myid, nproc
    integer, dimension(mpi_status_size) :: status

    integer(4) :: n, i, j, b, band_num, band_len, step
	integer, parameter :: k=10, & ! количество шагов между записями в файл
						max_step=100 ! максимальное количество шагов
    real(mp), allocatable :: qh2(:,:), qh2_crop(:,:), u(:,:), u_crop(:,:), border(:)
    
    call mpi_init(err)
    call mpi_comm_size(mpi_comm_world, nproc, err)
    call mpi_comm_rank(mpi_comm_world, myid, err)
	band_num = nproc - 1 ! количество потоков, обрабатывающих поля

	if (myid == 0) then ! чтение источника в координаторе
		open(1, file='sources.dat', status='old')
		    read(1,'(2x, i5)') n
		    allocate(qh2(n, n), u(n, n))
		    read(1,*) qh2
			band_len = n / band_num
		close(1)
		! нулевая итерация
		u = 0
		call save_file(u, 0)
		! отправляем базовые параметры и qh2_crop по потокам
		do b=1,band_num
			call mpi_send(band_len, 1, mpi_integer4, b, 100, mpi_comm_world, err)
			call mpi_send(n, 1, mpi_integer4, b, 200, mpi_comm_world, err)
			call mpi_send(qh2(band_len*(b-1)+1:band_len*b, :), band_len*n, mpi_real4, b, 666, mpi_comm_world, err)
		end do
	else
		! получаем базовые параметры в потоках
		call mpi_recv(band_len, 1, mpi_integer4, 0, 100, mpi_comm_world, status, err)
		call mpi_recv(n, 1, mpi_integer4, 0, 200, mpi_comm_world, status, err)
		! получаем и записываем поле qh2_crop
		allocate(qh2_crop(band_len, n))
		call mpi_recv(qh2_crop, band_len*n, mpi_real4, 0, 666, mpi_comm_world, status, err)
		! размещаем поля
		allocate(u_crop(0:band_len+1, 0:n+1), border(n))
		u_crop = 0
		border = 0
	end if

	! начинаем итерации
	do step=1,max_step
		
		if (myid == 0) then
			write(*,*) 'Координирующий поток: шаг '//str(step)//' начался'
		else
			if (step /= 1) then ! обмен границами со второго шага
				if (myid == 1) then ! это левое поле
					!write(*,*) 'Поток 1: попытка обмена с потоком 2...'
					border = u_crop(band_len, :)
					call mpi_sendrecv_replace(border, n, mpi_real4, 2, 000, 2, 000, mpi_comm_world, status, err)
					u_crop(band_len+1, :) = border
				else if (myid == band_num) then ! это правое поле
					!write(*,*) 'Поток '//str(myid)//': попытка обмена с потоком '//str(band_num-1)//'...'
					border = u_crop(1, :)
					call mpi_sendrecv_replace(border, n, mpi_real4, band_num-1, 000, band_num-1, 000, mpi_comm_world, status, err)
					u_crop(0, :) = border
				else ! это внутреннее поле
					!write(*,*) 'Поток '//str(myid)//': (1/2) попытка обмена с потоком '//str(myid-1)//'...'
					border = u_crop(1, :)
					call mpi_sendrecv_replace(border, n, mpi_real4, myid-1, 000, myid-1, 000, mpi_comm_world, status, err)
					u_crop(0, :) = border
					!write(*,*) 'Поток '//str(myid)//': (2/2) попытка обмена с потоком '//str(myid+1)//'...'
					border = u_crop(band_len, :)
					call mpi_sendrecv_replace(border, n, mpi_real4, myid+1, 000, myid+1, 000, mpi_comm_world, status, err)
					u_crop(band_len+1, :) = border
				end if
				write(*,*) 'Поток '//str(myid)//': успешный обмен'
			end if

			forall (i=1:band_len, j=1:n)
				u_crop(i, j) = (u_crop(i+1, j) + u_crop(i-1, j) + u_crop(i, j+1) + u_crop(i, j-1) + qh2_crop(i, j)) / 4
			end forall
		end if

		call mpi_barrier(mpi_comm_world, err)

		if (mod(step, k) == 0) then ! каждые k шагов
			if (myid /= 0) then ! отправляем поле
				call mpi_send(u_crop(1:band_len, 1:n), band_len*n, mpi_real4, 0, 999, mpi_comm_world, err)
				write(*,*) 'Поток '//str(myid)//': поле отправлено'
			else ! получаем поля
				write(*,*) 'Координирующий поток: попытка получения полей'
				do b=1,band_num
					call mpi_recv(u(band_len*(b-1)+1:band_len*b, :), band_len*n, mpi_real4, b, 999, mpi_comm_world, status, err)
				end do
				write(*,*) 'Координирующий поток: все поля получены'
				call save_file(u, step)
			end if
		end if

		call mpi_barrier(mpi_comm_world, err)
	end do

    call mpi_finalize(err)

	contains

	subroutine save_file(array, indx)
        real(mp), intent(in) :: array(:,:)
		integer :: m, indx, row
		m = size(array, dim=2)
		open(1, file='data/data'//str(indx)//'.dat')
			write(1,'("# ", i0)') m
			do row = 1,m
				write(1,'('//str(m)//'(f10.'//str(dp)//'))') array(:, row)
			end do
		close(1)
	end subroutine

end program
