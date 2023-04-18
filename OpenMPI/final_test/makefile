name := main
comp := mpif90
opt := -c -O2 -fcheck=all -ffree-line-length-none
ext := f95

result:
		mpif90 *.f95 -o $(name)
		mpiexec -np 4 -host localhost:4 ./$(name)

clear:
		rm -f *.o *.mod $(name)
