a.out: y.tab.c lex.yy.c
	gcc -O3 lex.yy.c y.tab.c
	@echo "Run the program as ./a.out [input_file]"

y.tab.c: a1.y 
	yacc -d a1.y

lex.yy.c: a1.l y.tab.h
	lex a1.l

clean:
	@rm -f lex.yy.c y.tab.h y.tab.c a.out

