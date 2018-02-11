OCBFLAGS :=
OCB := ocamlbuild $(OCBFLAGS)

.PHONY: all debug clean top

all: compiler.native
debug: all compiler.cma

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
