RPACKAGE=rmethods

all: _build/$(RPACKAGE).otarget byte native

clean:
	rm -f *.o *.cmo *.cmx
	ocamlbuild -clean

byte: _build/$(RPACKAGE).byte.otarget
native: _build/$(RPACKAGE).native.otarget

_build/$(RPACKAGE).otarget: $(RPACKAGE).itarget
	ocamlbuild -no-hygiene $(RPACKAGE).otarget

_build/$(RPACKAGE).byte.otarget: $(RPACKAGE).byte.itarget
	ocamlbuild -no-hygiene $(RPACKAGE).byte.otarget

_build/$(RPACKAGE).native.otarget: $(RPACKAGE).native.itarget
	ocamlbuild -no-hygiene $(RPACKAGE).native.otarget
