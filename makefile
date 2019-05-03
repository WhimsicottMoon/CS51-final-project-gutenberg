all: input-output parseit

input-output: input-output.ml
	ocamlbuild -use-ocamlfind input-output.byte

parseit: parseit.ml
	ocamlbuild -use-ocamlfind parseit.byte

clean:
	rm -rf _build *.byte