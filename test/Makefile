.SECONDARY:
.PRECIOUS: %.ll

all: npiet-foogol.bin hello.bin hi.bin nfib.bin nhello.bin loop.bin nprime.bin

clean:
	rm -rf *.bin *.s *.ll

%.bin: %.s
	g++ -O3 $^ -o $@

%.s: %.ll
	llc -tailcallopt -O3 $^ -o $@

%.ll: %.png
	../llvm-piet.py -OSvo $@ $^
