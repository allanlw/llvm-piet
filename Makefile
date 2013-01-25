# Makefile for hello pass

all: piet-passes.so pushpop.ll

piet-passes.o: piet-passes.cc
	g++ -o $@ $^ -c -g -fPIC -O2 -Wall `llvm-config-3.0 --cxxflags` -Wextra

piet-passes.so: piet-passes.o
	g++ -o $@ -shared $^ -Wl,-soname,$@ -fPIC -Wall -g

# the sed command is a hack because llvm-piet only supports llvm 3.0
pushpop.ll: pushpop.cc
#	llvmc-2.8 -o $@ $^ -emit-llvm -fplugin-arg-dragonegg-emit-ir -S
	clang -o - $^ -emit-llvm -S -O3 -fno-exceptions | sed "s/unnamed_addr//g" | sed "s/uwtable//g" > $@

#ops.ll:
#	python ops.py > ops.ll

clean:
	rm -rf piet-passes.so piet-passes.o pushpop.ll *.pyc
