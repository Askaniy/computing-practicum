module_name := fortran.cpython-310-x86_64-linux-gnu.so
debug_options := -fcheck=all
optim_options := -Ofast
opt := -ffree-line-length-none -fopenmp

fast: opt += $(optim_options)
fast: result

debug: opt += $(debug_options)
debug: result

$(module_name): fortran.f95
		f2py -c fortran.f95 -m fortran --f90flags='$(opt)' -lgomp

result: $(module_name)
		python3 main.py

clear:
		rm -f *.so *.mod $(name)