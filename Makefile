
CXX=clang++
CXXFLAGS=-std=c++14 -ggdb -O0
ttt:

%.png: %.dot
	dot -Grankdir=LR -Tpng -o $@ $<

project: ttt
	./ttt --hero > hero.dot
	./ttt --villian > villian.dot
	dot -Tpng -o hero.png hero.dot
	dot -Grankdir=LR -Tpng -o villian.png villian.dot

clean:
	$(RM) ttt *.png *.dot
