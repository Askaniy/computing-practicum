module_name := fortran.cpython-310-x86_64-linux-gnu.so
debug_options := -fcheck=all
optim_options := -Ofast
opt := -ffree-line-length-none -fopenmp
nohup_cmd := 

fast: opt += $(optim_options)
fast: nohup_cmd += nohup
fast: result

debug: opt += $(debug_options)
debug: result

$(module_name): fortran.f95
		$(nohup_cmd) f2py -c fortran.f95 -m fortran --f90flags='$(opt)' -lgomp

result: $(module_name)
		python3 main.py

clear:
		rm -f *.so *.mod nohup.out $(name)