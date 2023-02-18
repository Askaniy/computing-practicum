name := main
comp := gfortran
opt := -c -O2 -fcheck=all -ffree-line-length-none 
ext := f95

files := $(wildcard *.$(ext))
obj := $(patsubst %.$(ext), %.o, $(files))

$(name): $(obj)
		$(comp) $^ -o $@

%.o %.mod: %.$(ext)
		$(comp) $(opt) $<
		touch $@

$(name).o: my_prec.mod my_io.mod

result: $(name) input
		./$(name) < input > result

clear:
		rm -f *.o *.mod $(name)