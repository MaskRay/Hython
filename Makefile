.PHONY: all clean dist test Main

ifdef OPT
ALEXFLAGS := -g
HAPPYFLAGS := -acg
endif

all: Main

%.hs: %.x
	alex $(ALEXFLAGS) $<

%.hs: %.y
	happy $(HAPPYFLAGS) $<

Main: Parser/Lexer.hs Parser/Parser.hs
	ghc -i$(dir $(PWD)) -O2 --make $@

clean:
	$(RM) $(foreach d,Parser Core,$(wildcard $d/*.hi $d/*.o $d/*.info)) $(foreach s,x y,$(patsubst %.$s,%.hs,$(wildcard Parser/*.$s)))

dist:
	git archive --prefix Hython/ HEAD -o Hython.tar.gz

test: $(wildcard tests/*.in) Main
	@$(foreach t,$(filter %.in,$^),echo -e '\e[035mrunning test $(notdir $t)\e[0m'; ./Main $t | diff - $(t:.in=.out) && echo -e '\e[032mpassed\0[0m';)
