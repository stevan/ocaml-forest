
## NOTE:
## evaluate me for the side effects only ;)
FOO := $(shell /usr/bin/perl util/assemble.pl)

SOURCES = forest.ml
RESULT  = forest

TRASH = forest.ml

PACKS = extLib

LIBINSTALL_FILES = forest.mli forest.cmi \
		           forest.cma forest.cmxa forest.a

INCDIRS = /usr/local/lib/ocaml/site-lib/extlib /usr/local/lib/ocaml/
OCAMLBLDFLAGS = extLib.cma

all: bcl

opt: all ncl

test: all
	prove t/*.ml

-include OCamlMakefile