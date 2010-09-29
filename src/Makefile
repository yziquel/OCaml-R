#################################################################################
#                OCaml-R                                                        #
#                                                                               #
#    Copyright (C) 2008-2010 Institut National de Recherche en                  #
#    Informatique et en Automatique. All rights reserved.                       #
#                                                                               #
#    Copyright (C) 2009-2010 Guillaume Yziquel. All rights reserved.            #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License as                    #
#    published by the Free Software Foundation; either version 3 of the         #
#    License, or  any later version.                                            #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU General Public                  #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#             guillaume.yziquel@citycable.ch                                    #
#################################################################################

include ../master.Makefile

all: opt byte
opt: libopt mathopt
byte: lib math

libopt: r.cmxa r.cmxs oCamlR.cmx rbase.cmxa rstats.cmxa rmath.cmxa
lib: r.cma oCamlR.cmo rbase.cma rstats.cma rmath.cma

mathopt: rmath.cmxa
math: rmath.cma

rmath.cma: dllrmath_stubs.so math/rmath.cmo
	$(OCAMLC) -verbose -a -dllib dllrmath_stubs.so -dllib libRmath.so -o rmath.cma math/rmath.cmo

rmath.cmxa: dllrmath_stubs.so math/rmath.cmx
	$(OCAMLOPT) -verbose -a -cclib -lrmath_stubs -cclib -lRmath -o rmath.cmxa math/rmath.cmx

librmath_stubs.a: math/rmath_stubs.o
	ar rcs librmath_stubs.a math/rmath_stubs.o

dllrmath_stubs.so: librmath_stubs.a math/rmath_stubs.o
	ocamlmklib -verbose -o rmath_stubs math/rmath_stubs.o

math/rmath_stubs.o: math/rmath_stubs.c

r.cma: dllr_stubs.so r.cmo
	$(OCAMLC) -verbose -a -dllpath $(RLIBDIR) -dllib dllr_stubs.so -dllib libR.so -o r.cma r.cmo

r.cmxa: dllr_stubs.so r.cmx
	$(OCAMLOPT) -verbose -a -ccopt -L$(RLIBDIR) -cclib -lr_stubs -cclib -lR -o r.cmxa r.cmx

rbase.cma: rbase.cmo
	$(OCAMLC) -verbose -a -o rbase.cma rbase.cmo

rbase.cmxa: rbase.cmx
	$(OCAMLOPT) -verbose -a -o rbase.cmxa rbase.cmx

rstats.cma: rstats.cmo
	$(OCAMLC) -verbose -a -o rstats.cma rstats.cmo

rstats.cmxa: rstats.cmx
	$(OCAMLOPT) -verbose -a -o rstats.cmxa rstats.cmx

R_ML_SOURCES=\
	  r_Env.ml              \
	  standard.ml           \
	  data.ml               \
	  sexptype.ml           \
	  sexprec.ml            \
	  allocation.ml         \
	  read_internal.ml      \
	  write_internal.ml     \
	  lazy.ml               \
	  symbols.ml            \
	  conversion.ml         \
	  internal.ml           \
	  s3.ml                 \
	  s4.ml                 \
	  parser.ml             \
	  reduction.ml          \
	  initialisation.ml

r.ml: r.mli $(R_ML_SOURCES)
	cat $(R_ML_SOURCES) > $@

r.mli: \
	  incipit.mli           \
	  r_Env.mli             \
	  standard.mli          \
	  data.mli              \
	  sexptype.mli          \
	  symbols.mli           \
	  conversion.mli        \
	  internal.mli          \
	  s3.mli                \
	  s4.mli                \
	  parser.mli            \
	  reduction.mli         \
	  initialisation.mli
	cat $^ > $@

standard.ml: standard.R
	R --silent --vanilla --slave < $< > $@

RBASE_ML_SOURCES= \
	  r-base/main.ml        \
	  r-base/listing.ml     \
	  r-base/dataFrame.ml   \
	  r-base/date.ml
rbase.ml: rbase.mli $(RBASE_ML_SOURCSE)
	cat $(RBASE_ML_SOURCES) > $@

rbase.mli: \
	  r-base/main.mli       \
	  r-base/listing.mli    \
	  r-base/dataFrame.mli  \
	  r-base/date.mli
	cat $^ > $@

rstats.ml: rstats.mli r-stats/main.ml
	cat r-stats/main.ml > $@

rstats.mli: r-stats/main.mli
	cat $^ > $@

libr_stubs.a: r_stubs.o
	ar rcs libr_stubs.a r_stubs.o

dllr_stubs.so: libr_stubs.a r_stubs.o
	$(OCAMLMKLIB) -verbose -o r_stubs r_stubs.o

clean:
	$(MAKE) -C math clean
	rm -f standard.ml base.ml base.mli r.ml r.mli rbase.ml rbase.mli rstats.ml rstats.mli
	rm -f *.o *.so *.a *.cmi *.cmo *.cmx *.cma *.cmxa *.cmxs *.annot

test: all
	$(OCAML) -init ocamlinit

doc: r.mli rbase.mli rstats.mli math/rmath.mli
	$(MKDIR) ocamldoc
	$(OCAMLFIND) ocamldoc -package calendar $(INCLUDES) \
	-html -d ocamldoc $^

install: all remove
	$(OCAMLFIND) install R META *.a *.cm[ai] *.cmxa dllr_stubs.so

remove:
	$(OCAMLFIND) remove R

