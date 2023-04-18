C**********************************************************************
C     Benchmark #2 -- Double  Precision Whetstone (A001)
C
C     o	This is a REAL*8 version of
C	the Whetstone benchmark program.
C
C     o	DO-loop semantics are ANSI-66 compatible.
C
C     o	Final measurements are to be made with all
C	WRITE statements and FORMAT sttements removed.
C
C**********************************************************************   
        subroutine WHETSTONE(LOOP,II)
	implicit real*8 (A-H,O-Z)
C
	common T,T1,T2,E1(4),J,K,L
	common/ptime/ptime,time0
	real time0,time1,walltime,ptime
	integer LOOP,II
C
c	WRITE(6,1)
c   1	FORMAT(/' Benchmark #2 -- Double Precision Whetstone (A001)')
C
C	Start benchmark timing at this point.
C
	time0 = walltime()
	ptime = time0
C
C	The actual benchmark starts here.
C
	T = .499975
	T1 = 0.50025
	T2 = 2.0
C
C	With loopcount LOOP=10, one million Whetstone instructions
C	will be executed in EACH MAJOR LOOP..A MAJOR LOOP IS EXECUTED
C	'II' TIMES TO INCREASE WALL-CLOCK TIMING ACCURACY.
C
C	LOOP = 10000
c	II   = 1
C
	do 500 JJ=1,II
C
C	Establish the relative loop counts of each module.
C
	N1 = 0
	N2 = 12 * LOOP
	N3 = 14 * LOOP
	N4 = 345 * LOOP
	N5 = 0
	N6 = 210 * LOOP
	N7 = 32 * LOOP
	N8 = 899 * LOOP
	N9 = 616 * LOOP
	N10 = 0
	N11 = 93 * LOOP
C
C	Module 1: Simple identifiers
C
	X1 = 1.0
	X2 = -1.0
	X3 = -1.0
	X4 = -1.0
C
	if (N1.EQ.0) goto 35
		do 30 I=1,N1
		X1 = (X1 + X2 + X3 - X4)*T
		X2 = (X1 + X2 - X3 + X4)*T
		X3 = (X1 - X2 + X3 + X4)*T
		X4 = (-X1 + X2 + X3 + X4)*T
   30		continue
   35	continue
C
	if (JJ.EQ.II) call POUT(N1,N1,N1,X1,X2,X3,X4)
C
C	Module 2: Array elements
C
	E1(1) = 1.0
	E1(2) = -1.0
	E1(3) = -1.0
	E1(4) = -1.0
C
	if (N2.EQ.0) goto 45
		do 40 I=1,N2
		E1(1) = (E1(1) + E1(2) + E1(3) - E1(4))*T
		E1(2) = (E1(1) + E1(2) - E1(3) + E1(4))*T
		E1(3) = (E1(1) - E1(2) + E1(3) + E1(4))*T
		E1(4) = (-E1(1) + E1(2) + E1(3) + E1(4))*T
   40		continue
   45	continue
C
	if (JJ.EQ.II) call POUT(N2,N3,N2,E1(1),E1(2),E1(3),E1(4))
C
C	Module 3: Array as parameter
C
	if (N3.EQ.0) goto 59
		do 50 I=1,N3
		call PA(E1)
   50		continue
   59	continue
C
	if (JJ.EQ.II) call POUT(N3,N2,N2,E1(1),E1(2),E1(3),E1(4))
C
C	Module 4: Conditional jumps
C
	J = 1
	if (N4.EQ.0) goto 65
		do 60 I=1,N4
		if (J.EQ.1) goto 51
		J = 3
		goto 52
51		J = 2
52		if (J.gt.2) goto 53
		J = 1
		goto 54
53		J = 0
54		if (J.LT.1) goto 55
		J = 0
		goto 60
55		J = 1
   60		continue
   65	continue
C
	if (JJ.EQ.II) call POUT(N4,J,J,X1,X2,X3,X4)
C
C	Module 5: Omitted
C 	Module 6: Integer arithmetic
C	
	J = 1
	K = 2
	L = 3
C
	if (N6.EQ.0) goto 75
		do 70 I=1,N6
		J = J * (K-J) * (L-K)
		K = L * K - (L-J) * K
		L = (L - K) * (K + J)
		E1(L-1) = J + K + L
		E1(K-1) = J * K * L
   70		continue
   75	continue
C
	if (JJ.EQ.II) call POUT(N6,J,K,E1(1),E1(2),E1(3),E1(4))
C
C	Module 7: Trigonometric functions
C
	X = 0.5
	Y = 0.5
C
	if (N7.EQ.0) goto 85
		do 80 I=1,N7
		X=T*ATAN(T2*SIN(X)*COS(X)/(COS(X+Y)+COS(X-Y)-1.0))
		Y=T*ATAN(T2*SIN(Y)*COS(Y)/(COS(X+Y)+COS(X-Y)-1.0))
   80		continue
   85	continue
C
	if (JJ.EQ.II) call POUT(N7,J,K,X,X,Y,Y)
C
C	Module 8: Procedure calls
C
	X = 1.0
	Y = 1.0
	Z = 1.0
C
	if (N8.EQ.0) goto 95
		do 90 I=1,N8
		call P3(X,Y,Z)
   90 		continue
   95	continue
C
	if (JJ.EQ.II) call POUT(N8,J,K,X,Y,Z,Z)
C
C	Module 9: Array references
C
	J = 1
	K = 2
	L = 3
	E1(1) = 1.0
	E1(2) = 2.0
	E1(3) = 3.0
C
	if (N9.EQ.0) goto 105
		do 100  I=1,N9
		call P0
  100		continue
  105	continue
C
	if (JJ.EQ.II) call POUT(N9,J,K,E1(1),E1(2),E1(3),E1(4))
C
C	Module 10: Integer arithmetic
C
	J = 2
	K = 3
C
	if (N10.EQ.0) goto 115
		do 110 I=1,N10
		J = J + K
		K = J + K
		J = K - J
		K = K - J - J
  110		continue
  115	continue
C
	if (JJ.EQ.II) call POUT(N10,J,K,X1,X2,X3,X4)
C
C	Module 11: Standard functions
C
	X = 0.75
C
	if (N11.EQ.0) goto 125
		do 120 I=1,N11
		X = SQRT(EXP(LOG(X)/T1))
  120		continue
  125	continue
C
	if (JJ.EQ.II) call POUT(N11,J,K,X,X,X,X)
C
C      THIS IS THE END OF THE MAJOR LOOP.
C
500	continue
C
C      Stop benchmark timing at this point.
C
	time1 = walltime()
C----------------------------------------------------------------
C      Performance in Whetstone KIP's per second is given by
C
C	(100*LOOP*II)/TIME
C
C      where TIME is in seconds.
C--------------------------------------------------------------------
c	print *,'MWIPS ',nint((100*LOOP*II)/(time1-time0)/1000)
c	MWIPS=nint(((100*LOOP*II)/(time1-time0))/1000)
	return
	end
C
	subroutine PA(E)
	implicit real*8 (A-H,O-Z)
	dimension E(4)
	common T,T1,T2,E1(4),J,K,L
	J1 = 0
   10	E(1) = (E(1) + E(2) + E(3) - E(4)) * T
	E(2) = (E(1) + E(2) - E(3) + E(4)) * T  
	E(3) = (E(1) - E(2) + E(3) + E(4)) * T
	E(4) = (-E(1) + E(2) + E(3) + E(4)) / T2
	J1 = J1 + 1
	if (J1 - 6) 10,20,20
C
   20	return
	end
C
	subroutine P0
	implicit real*8 (A-H,O-Z)
	common T,T1,T2,E1(4),J,K,L
	E1(J) = E1(K)
	E1(K) = E1(L)
	E1(L) = E1(J)
	return
	end
C
	subroutine P3(X,Y,Z)
	implicit real*8 (A-H,O-Z)
	common T,T1,T2,E1(4),J,K,L
	X1 = X
	Y1 = Y
	X1 = T * (X1 + Y1)
	Y1 = T * (X1 + Y1)
	Z = (X1 + Y1) / T2
	return
	end
C
	subroutine POUT(N,J,K,X1,X2,X3,X4)
	implicit real*8 (A-H,O-Z)
	common/ptime/ptime,time0
	real ptime,time1,time0,walltime
	time1 = walltime()
c	print 10, nint(time1-time0),nint(time1-ptime),N,J,K,X1,X2,X3,X4
c   10	FORMAT (2i3,1X,3I7,4(1PE12.4))
	ptime = time1
	return
	end

        real function walltime() 
        real it(2)
        walltime = etime(it)
        return
        end

