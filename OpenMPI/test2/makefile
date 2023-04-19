name := main
comp := mpif90
opt := -c -fcheck=all -ffree-line-length-none

result:
		mpif90 my_io.f95 my_math.f95 main.f95 -o $(name)
		mpiexec -np 4 -host localhost:4 ./$(name)

clear:
		rm -f *.o *.mod $(name)
