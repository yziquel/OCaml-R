RPACKAGE=rbase

all: _build/$(RPACKAGE).otarget byte native

clean:
	rm -f $(RPACKAGE).ml $(RPACKAGE).mli *.o *.cmo *.cmx *.cmi
	ocamlbuild -clean

byte: _build/$(RPACKAGE).byte.otarget
native: _build/$(RPACKAGE).native.otarget

_build/$(RPACKAGE).otarget: $(RPACKAGE).ml $(RPACKAGE).mli $(RPACKAGE).itarget
	cp ../r.cmi .
	ocamlbuild -no-hygiene $(RPACKAGE).otarget
	rm -f r.cmi

_build/$(RPACKAGE).byte.otarget: $(RPACKAGE).ml $(RPACKAGE).mli $(RPACKAGE).byte.itarget
	cp ../r.cmi .
	ocamlbuild -no-hygiene $(RPACKAGE).byte.otarget
	rm -f r.cmi

_build/$(RPACKAGE).native.otarget: $(RPACKAGE).ml $(RPACKAGE).mli $(RPACKAGE).native.itarget
	cp ../r.cmi .
	ocamlbuild -no-hygiene $(RPACKAGE).native.otarget
	rm -f r.cmi

$(RPACKAGE).ml: main.ml
	camlp4o -o $(RPACKAGE).ml -printer Camlp4OCamlPrinter -parser Camlp4MacroParser main.ml

$(RPACKAGE).mli: main.mli
	camlp4o -o $(RPACKAGE).mli -printer Camlp4OCamlPrinter -parser Camlp4MacroParser main.mli
