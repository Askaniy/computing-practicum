name := main

result:
		mpif90 my_io.f95 my_math.f95 main.f95 -fcheck=all -ffree-line-length-none -o $(name)
		mpiexec -np 4 -host localhost:4 ./$(name)
		python3 -u to_gif.py

clear:
		rm -f *.o *.mod $(name) ./data/*.dat
