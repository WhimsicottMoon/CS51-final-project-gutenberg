all: gutenberg

gutenberg: gutenberg.ml
	ocamlfind opt -package plplot -linkpkg -o gutenberg.byte gutenberg.ml

clean:
	rm -rf _build *.byte
