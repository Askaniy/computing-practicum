name := main
comp := gfortran
debug_options := -fcheck=all
optim_options := -Ofast
opt := -ffree-line-length-none
ext := f95
mode = 

files := $(wildcard *.$(ext))
obj := $(patsubst %.$(ext), %.o, $(files))

optimize: opt += $(optim_options)
optimize: result

debug: opt += $(debug_options)
debug: result

$(name): $(obj)
		$(comp) $^ -o $@

%.o %.mod: %.$(ext)
		$(comp) -c $(opt) $<
		touch $@

$(name).o: my_io.mod my_math.mod

result: $(name)
		time -v -o time.log ./$(name) $(mode)

clear:
		rm -f *.o *.mod $(name) time.log