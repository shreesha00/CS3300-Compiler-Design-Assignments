a.out: y.tab.cpp lex.yy.c
	g++ -O3 lex.yy.c y.tab.cpp
	@echo "Run the program as ./a.out <[input_file]"

y.tab.cpp: a4.y
	yacc -d a4.y -o y.tab.cpp

lex.yy.c: a4.l y.tab.hpp
	lex a4.l

clean:
	@rm -f lex.yy.c y.tab.hpp y.tab.cpp a.out
