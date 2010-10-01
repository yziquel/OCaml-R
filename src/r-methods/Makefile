RPACKAGE=rmethods

all: _build/$(RPACKAGE).otarget byte native

clean:
	rm -f *.o *.cmo *.cmx
	ocamlbuild -clean

byte: _build/$(RPACKAGE).byte.otarget $(RPACKAGE)Hook.cmo
native: _build/$(RPACKAGE).native.otarget $(RPACKAGE)Hook.cmx

$(RPACKAGE)Hook.cmo: _build/$(RPACKAGE).byte.otarget $(RPACKAGE)Hook.ml
	ocamlfind ocamlc -I .. -c $(RPACKAGE)Hook.ml
	rm -f $(RPACKAGE)Hook.cmi

$(RPACKAGE)Hook.cmx: _build/$(RPACKAGE).native.otarget $(RPACKAGE)Hook.ml
	ocamlfind ocamlopt -I .. -c $(RPACKAGE)Hook.ml
	rm -f $(RPACKAGE)Hook.cmi

_build/$(RPACKAGE).otarget: $(RPACKAGE).itarget
	ocamlbuild -no-hygiene $(RPACKAGE).otarget

_build/$(RPACKAGE).byte.otarget: $(RPACKAGE).byte.itarget
	ocamlbuild -no-hygiene $(RPACKAGE).byte.otarget

_build/$(RPACKAGE).native.otarget: $(RPACKAGE).native.itarget
	ocamlbuild -no-hygiene $(RPACKAGE).native.otarget
