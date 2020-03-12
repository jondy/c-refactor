all: c

clean: clean_c

clex: c.lex.yy.c
	gcc -DFLEX -o $@ c.lex.yy.c

cparse: base_c.tab.c base_c.tab.h c.lex.yy.c
	gcc -I. -o $@ base_c.tab.c c.lex.yy.c

c.lex.yy.c: c.lex base_c.tab.h
	flex -o c.lex.yy.c c.lex

base_c.tab.c base_c.tab.h: base_c.y
	bison -d -v -o base_c.tab.c base_c.y

clean_c:
	rm pparse.exe base_c.tab.c base_c.tab.h c.lex.yy.c
