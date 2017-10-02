
CXX=clang++
CXXFLAGS=-std=c++14 -ggdb -O0
ttt:

%.png: %.dot
	dot -Tpng -o $@ $<
