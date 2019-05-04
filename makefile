all: ioit parseit

ioit: ioit.ml
	ocamlbuild -use-ocamlfind ioit.byte

parseit: parseit.ml
	ocamlbuild -use-ocamlfind parseit.byte

ahhh: ahh.ml
	ocamlbuild -use-ocamlfind ahhh.byte

clean:
	rm -rf _build *.byte
