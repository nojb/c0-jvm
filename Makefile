OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all:
	$(OCAMLBUILD) src/jvm.byte

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean
