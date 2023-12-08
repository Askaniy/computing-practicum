name := main
comp := gfortran
opt := -ffree-line-length-none
ext := f95
mode = 

files := $(wildcard *.$(ext))
obj := $(patsubst %.$(ext), %.o, $(files))

debug: opt += -fcheck=all
debug: result

optimize: opt += -Ofast
optimize: result

$(name): $(obj)
		$(comp) $^ -o $@

%.o %.mod: %.$(ext)
		$(comp) -c $(opt) $<
		touch $@

$(name).o: my_io.mod my_math.mod

result: $(name)
		time -v -o time.log ./$(name) $(mode)
#		python3 -u plot.py

clean:
		rm -f *.o *.mod $(name) time.log