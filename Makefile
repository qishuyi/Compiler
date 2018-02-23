OCBFLAGS :=
OCB := ocamlbuild $(OCBFLAGS)

.PHONY: all test debug clean top

all: compiler.native
debug: all compiler.cma

test:
	./test/test.sh

%.cma: .FORCE
	$(OCB) $@

%.cmxa: .FORCE
	$(OCB) $@

%.native: .FORCE
	$(OCB) $@

%.p.native: .FORCE
	$(OCB) $@

%.byte: .FORCE
	$(OCB) $@

.FORCE:

clean:
	$(OCB) -clean
	rm -f *.cm[iox] *~

top: compiler.cma
	utop
