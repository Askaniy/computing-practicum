debug_options := -fcheck=all
optim_options := -Ofast
opt := -ffree-line-length-none -fopenmp

fast: opt += $(optim_options)
fast: result

debug: opt += $(debug_options)
debug: result

result:
		f2py -c fortran.f95 -m fortran --f90flags='$(opt)' -lgomp
		python3 main.py

clear:
		rm -f *.so *.mod $(name)