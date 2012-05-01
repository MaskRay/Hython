.PHONY: all clean dist test Main

all: Main

%.hs: %.x
	alex $<

%.hs: %.y
	happy $<

Main:
	ghc -i$$HOME --make $@

clean:
	$(RM) $(foreach d,Parser Test,$(wildcard $d/*.hi $d/*.o $d/*.info)) $(foreach s,x y,$(patsubst %.$s,%.hs,$(wildcard Parser/*.$s)))

dist:
	git archive --prefix Hython/ HEAD -o Hython.tar.gz

test: $(wildcard tests/*.in) Main
	@$(foreach t,$(filter %.in,$^),./Main - < $t | diff - $(t:.in=.out) && echo "test $(notdir $t) passed";)
