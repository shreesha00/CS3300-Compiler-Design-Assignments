a.out: y.tab.c lex.yy.c
	gcc -O3 lex.yy.c y.tab.c
	@echo "Run the program as ./a.out <input.txt"

y.tab.c: a2.y 
	yacc -d a2.y

lex.yy.c: a2.l y.tab.h
	lex a2.l

clean:
	@rm lex.yy.c y.tab.h y.tab.c a.out