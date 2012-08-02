.SECONDARY:
.PRECIOUS: %.ll

PNGS = $(sort $(wildcard *.png) $(patsubst %.foo, %.png, $(wildcard *.foo)))
TESTS = $(patsubst %.png, %.bin, $(PNGS))

all: $(TESTS)

clean:
	rm -rf *.bin *.s *.ll

%.bin: %.s
	g++ -O3 $^ -o $@

%.s: %.ll
	llc -tailcallopt -O3 $^ -o $@

%.ll: %.png
	../llvm-piet.py --lowerswitch --no-simplify-libcalls -Ovo $@ $^

%.png: %.foo
	npiet-foogol -O -o $@ $^
