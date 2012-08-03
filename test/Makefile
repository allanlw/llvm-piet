.SECONDARY:
.PRECIOUS: %.ll

PNGS = $(sort $(wildcard *.png) $(patsubst %.foo, %.png, $(wildcard *.foo)))
TESTS = $(patsubst %.png, %.bin, $(PNGS))

all: $(TESTS)

clean:
	rm -rf *.bin *.s *.ll*

clean-inter:
	rm -rf *.ll.*

%.bin: %.s
	g++ -O3 $^ -o $@

%.s: %.ll
	llc -tailcallopt -O3 $^ -o $@

%.ll: %.ll.ns %.ll.ls
	cp `wc -l $@.ns $@.ls | sort -n | head -n 1 | awk '{print $$2}'` $@

%.ll.ns: %.png
	../llvm-piet.py --no-simplify-libcalls -Ovo $@ $^

%.ll.ls: %.png
	../llvm-piet.py --no-simplify-libcalls --lowerswitch -Ovo $@ $^

%.png: %.foo
	npiet-foogol -O -o $@ $^
