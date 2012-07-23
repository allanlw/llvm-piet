# Makefile for hello pass

all: piet-passes.o piet-passes.so pushpop.ll

piet-passes.o: piet-passes.cc
	g++ -o $@ $^ -c -g -fPIC -O2 -Wall `llvm-config-2.8 --cxxflags` `python2.7-config --includes`

piet-passes.so: piet-passes.o
	g++ -o $@ -shared $^ -Wl,-soname,$@ -fPIC -Wall -g `llvm-config-2.8 --libs --ldflags` `python2.7-config --libs --ldflags`

pushpop.ll: pushpop.cc
#	llvmc-2.8 -o $@ $^ -emit-llvm -fplugin-arg-dragonegg-emit-ir -S
	clang -o - $^ -emit-llvm -S -O3 -fno-exceptions | sed "s/unnamed_addr//g" > $@

clean:
	rm -rf piet-passes.so piet-passes.o pushpop.ll *.pyc
