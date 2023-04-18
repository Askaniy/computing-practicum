      program calc_mwips
      include 'mpif.h'
      integer i, j, LOOP, II, ITER
      integer*8 ws_loc, ws_rank, ws_all
      real alltime1, alltime2
      real time1,time2,time_sum
      character (LEN=MPI_MAX_PROCESSOR_NAME) pname
      integer xmax,min_speed,BC,exc,status(MPI_STATUS_SIZE)
c     Длина границы разностной сетки
      parameter (xmax=8800)
      real*8 f0(xmax), f1(xmax)

c     Количество циклов теста Whetstone
      LOOP=10000
c     Умножитель цикла теста
      II=10
c     Количество итераций
      ITER=60
c     Количество массивов с границами обмена
      BC=6

c     Инициализируем массивы обмена
      f0=0.0
      f1=0.0

c     Инициализация MPI и определение процессорной конфигурации
      call MPI_INIT( ierr )
      call MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )
      call MPI_COMM_SIZE( MPI_COMM_WORLD, np, ierr )
      call MPI_GET_PROCESSOR_NAME(pname,pname_len,ierr)

      call MPI_BARRIER(MPI_COMM_WORLD,ierr)
      if (myid .eq. 0) alltime0=walltime()

      time_sum=0.0
      if (myid .eq. 0) alltime1=walltime()
      do 10 i = 1, ITER
         time1=walltime()
         call WHETSTONE(LOOP,II)
         time2=walltime()
         time_sum=time_sum+(time2-time1);
c        Обмениваемся границами с соседом
         if ( myid .gt. 0 ) then
           do 101 exc=1, BC
           call MPI_SENDRECV(
     x        f0(1), xmax, MPI_REAL8, myid-1, 1,
     x        f1(1), xmax, MPI_REAL8, myid-1, 1,
     x        MPI_COMM_WORLD, status, ierr)
  101      continue
         endif
         if ( myid .lt. np-1 ) then
           do 102 exc=1, BC
           call MPI_SENDRECV(
     x        f0(1), xmax, MPI_REAL8, myid+1, 1,
     x        f1(1), xmax, MPI_REAL8, myid+1, 1,
     x        MPI_COMM_WORLD, status, ierr)
  102      continue
         endif
c        Синхронизируем циклы
         if (np .gt. 1)
     $     call MPI_BARRIER(MPI_COMM_WORLD,ierr)
   10 continue
      if (myid .eq. 0) alltime2=walltime()

      ws_loc=((100.0*LOOP*II*ITER)/time_sum/1000.0)

c     Печать выходной информации с 0-го процессора
      if (myid .eq. 0) then

         ws_all=(((100.0*LOOP*II*ITER*np)/(alltime2-alltime1))
     $   /1000.0)
         print *, '   '
         print '(A)', 
     $   '  Cluster Double Precision Whetstone Test ---------'
         print '(A,I2.1)', '  Quantity of processors =  ', np
         print '(A,F6.2,A)',
     $   '  Calculation time       = ', (alltime2-alltime1), ' seconds'
c 	Внесена правка - сменил формат вывода
         print '(A,I8.1,A)',
     $   '  Cluster speed          = ', ws_all, ' MWIPS'
         print '(A)', 
     $   '  -------------------------------------------------'
         print '(A,I2.2,A,I6.1,A,A,A)', 
     $   '  Cluster node N',0,' speed = ',
     $   ws_loc, ' MWIPS (',trim(pname),')'
         min_speed=ws_loc
c        собираем значения быстродействия процессов
         do 20 i = 1, np-1
           call MPI_RECV(ws_rank, 1, MPI_INTEGER8, i, 0,
     $                   MPI_COMM_WORLD, status, ierr)
           call MPI_RECV(pname, MPI_MAX_PROCESSOR_NAME, MPI_CHARACTER,
     $          i,0,MPI_COMM_WORLD, status, ierr)
           print '(A,I2.2,A,I6.1,A,A,A)',
     $     '  Cluster node N', i,' speed = ',
     $     ws_rank, ' MWIPS (',trim(pname),')'
           min_speed=min(min_speed,ws_rank)
   20    continue
         print '(A)',
     $   '  -------------------------------------------------'
         print '(A,I6.1,A)',
     $   '  Nodes minimal speed    = ', min_speed, ' MWIPS'
         print '(A,F6.2)',
     $   '  Acceleration factor    = ', (ws_all*1.0)/(min_speed*1.0)
         print '(A)',
     $   '  -------------------------------------------------'
         print *, '   '

      else

c       посылаем значение быстродействия локального процесса
        call MPI_SEND(ws_loc, 1, MPI_INTEGER8, 0, 0,
     $                MPI_COMM_WORLD, ierr)
        call MPI_SEND(pname, MPI_MAX_PROCESSOR_NAME, MPI_CHARACTER,
     $                0,0,MPI_COMM_WORLD, ierr)

      endif

c     Закрытие MPI
      call MPI_FINALIZE(ierr)
      stop
      end
