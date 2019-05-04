all: ioit parseit firstsmoosh

ioit: ioit.ml
	ocamlbuild -use-ocamlfind ioit.byte

parseit: parseit.ml
	ocamlbuild -use-ocamlfind parseit.byte

firstsmoosh: firstsmoosh.ml
	ocamlbuild -use-ocamlfind firstsmoosh.byte

clean:
	rm -rf _build *.byte
