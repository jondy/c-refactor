BISON = /usr/local/opt/bison/bin/bison
FLEX = /usr/local/opt/flex/bin/flex
GCC = gcc

cparser: lex.yy.c c.tab.c ptypes.h
	$(GCC) -I. -o $@ lex.yy.c c.tab.c

clex: lex.yy.c
	$(GCC) -I. -DDEBUG_FLEX -o $@ lex.yy.c

lex.yy.c: c.l c.tab.h
	$(FLEX) c.l

c.tab.c c.tab.h: c.y ptypes.h
	$(BISON) -d -v c.y

clean:
	rm -rf cparser clex c.output c.tab.c c.tab.h lex.yy.c
