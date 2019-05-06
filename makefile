all: ioit parseit firstsmoosh gutenberg

ioit: ioit.ml
	ocamlbuild -use-ocamlfind ioit.byte

parseit: parseit.ml
	ocamlbuild -use-ocamlfind parseit.byte

firstsmoosh: firstsmoosh.ml
	ocamlbuild -use-ocamlfind firstsmoosh.byte

gutenberg: gutenberg.ml
	ocamlfind opt -package plplot -linkpkg -o gutenberg.byte gutenberg.ml

clean:
	rm -rf _build *.byte
