name := main
comp := gfortran
debug_options := -fcheck=all
optim_options := -Ofast
opt := -c -ffree-line-length-none
ext := f95

files := $(wildcard *.$(ext))
obj := $(patsubst %.$(ext), %.o, $(files))

optimize: opt += $(optim_options)
optimize: result

debug: opt += $(debug_options)
debug: result

$(name): $(obj)
		$(comp) $^ -o $@

%.o %.mod: %.$(ext)
		$(comp) $(opt) $<
		touch $@

$(name).o: my_io.mod my_math.mod

result: $(name)
		time -v -o time.log ./$(name)

clear:
		rm -f *.o *.mod $(name) time.log