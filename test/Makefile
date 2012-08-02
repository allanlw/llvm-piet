.SECONDARY:
.PRECIOUS: %.ll

all: short long

short: hello.bin hi.bin nfib.bin nhello.bin loop.bin nprime.bin 1.bin 2.bin 4.bin 5.bin \
	7.bin ciftest.bin ciftest2.bin empty.bin expr-0.bin expr-1.bin expr-2.bin fib.bin fox.bin \
	print.bin stack.bin stack2.bin stack3.bin while.bin while-if-1.bin

long: 99-bottles.bin brainfuck.bin foogol-example.bin

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
