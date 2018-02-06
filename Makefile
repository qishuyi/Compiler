OCBFLAGS := -use-ocamlfind
OCB := ocamlbuild $(OCBFLAGS)

project:
	$(OCB) project.native

test:
	./test.sh

clean:
	$(OCB) -clean
	rm -f *.cm[iox] *~
