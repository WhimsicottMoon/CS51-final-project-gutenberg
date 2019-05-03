all: input-output parseit

ioit: ioit.ml
	ocamlbuild -use-ocamlfind ioit.byte

parseit: parseit.ml
	ocamlbuild -use-ocamlfind parseit.byte

clean:
	rm -rf _build *.byte
