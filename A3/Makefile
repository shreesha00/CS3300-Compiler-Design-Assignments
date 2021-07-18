a.out: y.tab.cpp lex.yy.c
	g++ -O3 lex.yy.c y.tab.cpp
	@echo "Run the program as ./a.out <[input_file]"

y.tab.cpp: a3.y
	yacc -d a3.y -o y.tab.cpp

lex.yy.c: a3.l y.tab.hpp
	lex a3.l

clean:
	@rm -f lex.yy.c y.tab.hpp y.tab.cpp a.out
