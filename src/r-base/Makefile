RPACKAGE=rbase

all: _build/$(RPACKAGE).otarget byte native

clean:
	rm -f $(RPACKAGE).ml $(RPACKAGE).mli *.o *.cmo *.cmx *.cmi
	ocamlbuild -clean

byte: _build/$(RPACKAGE).byte.otarget
native: _build/$(RPACKAGE).native.otarget

_build/$(RPACKAGE).otarget: r.cmi $(RPACKAGE).ml $(RPACKAGE).mli $(RPACKAGE).itarget
	ocamlbuild -no-hygiene $(RPACKAGE).otarget

_build/$(RPACKAGE).byte.otarget: r.cmi $(RPACKAGE).ml $(RPACKAGE).mli $(RPACKAGE).byte.itarget
	ocamlbuild -no-hygiene $(RPACKAGE).byte.otarget

_build/$(RPACKAGE).native.otarget: r.cmi $(RPACKAGE).ml $(RPACKAGE).mli $(RPACKAGE).native.itarget
	ocamlbuild -no-hygiene $(RPACKAGE).native.otarget

r.cmi: ../r.cmi
	cp ../r.cmi r.cmi

$(RPACKAGE).ml: main.ml
	camlp4o -o $(RPACKAGE).ml -printer Camlp4OCamlPrinter -parser Camlp4MacroParser main.ml

$(RPACKAGE).mli: main.mli
	camlp4o -o $(RPACKAGE).mli -printer Camlp4OCamlPrinter -parser Camlp4MacroParser main.mli
