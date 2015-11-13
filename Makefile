OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all:
	$(OCAMLBUILD) src/main.byte

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean
