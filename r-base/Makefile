all: build

build: rbase.cma rbase.cmxa

rbase.cma: rbase.cmo
	ocamlc -verbose -o rbase.cma rbase.cmo

rbase.cmxa:
	ocamlopt -verbose -o rbase.cmxa rbase.cmx

rbase.cmi: rbase.mli
	ocamlfind ocamlc -verbose -c rbase.mli

rbase.cmo: rbase.ml rbase.cmi
	ocamlfind ocamlc -verbose -c rbase.ml

rbase.cmx: rbase.ml rbase.cmi
	ocamlfind ocamlopt -verbose -c rbase.ml

clean:
	rm -f *.o *.so *.a *.cmi *.cmo *.cmx *.cma *.cmxa *.cmxs
