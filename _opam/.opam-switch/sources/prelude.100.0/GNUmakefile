# prelude                             -*- makefile-gmake -*-
# GNUmakefile

ARCHIVE = ~/web/hg

SOURCES = a1.ml a1.mli ia.ml ia.mli prelude_time.ml prelude.ml
RESULT = prelude
# these packages are only used in unit tests
OPAMS_TEST = ctypes {with-test} ctypes-foreign {with-test} ocamlbuild {with-test} ounit2 {with-test} qcheck {with-test}
PACKS_TEST = $(filter-out ocamlbuild,$(shell erg pkg -n '$(PACKS) $(patsubst ctypes-foreign,ctypes.foreign,$(OPAMS_TEST))'))
OPAMS = camlp-streams $(OPAMS_TEST)
# LIB_PACK_NAME = $(RESULT)
PACKS = camlp-streams str unix
ifeq ($(LIB_PACK_NAME),)
  LIBINSTALL_FILES = $(wildcard *.[oa]) $(wildcard *.cm*) $(sort $(SOURCES))
else
  LIBINSTALL_FILES = $(wildcard $(RESULT).[ao]) $(wildcard $(RESULT).cm*) $(sort $(SOURCES))
endif

default: bcl

.PHONY: all
all: bcl ncl
run-ocaml: bcl

# TODO PROMOTE to erg, if  ocaml-makefile really doesn't have this dep
libinstall: META

.PHONY: install
install: libuninstall libinstall
.PHONY: uninstall
uninstall: libuninstall

TEST = test
$(TEST).ml: $(SOURCES)
	qtest -o $@ extract $^

OCAMLBUILD = ocamlbuild -use-ocamlfind -classic-display -j 8
TRASH += qtest.targets.log $(TEST) $(TEST).ml
.PHONY: check test
check test: clean $(TEST).ml
	$(OCAMLBUILD) $(patsubst %,-pkg %,$(PACKS)) -lflag -linkall $(RESULT).cmxa
	qtest -o $(TEST).ml extract a1.ml ia.ml prelude_time.ml prelude.ml
	$(OCAMLBUILD) -cflags -warn-error,+26 $(patsubst %,-pkg %,$(PACKS_TEST)) $(TEST).native
	./$(TEST).native
	$(OCAMLBUILD) -clean

clean::
	$(RM) opam
	$(OCAMLBUILD) -clean

ifdef DEBUG_BOOTSTRAP
  $(info DEBUG: Bootstrapping with ocaml-makefile...)
  OCAMLMAKEFILE = $(shell opam var ocaml-makefile:lib)/OCamlMakefile
  include $(OCAMLMAKEFILE)
else
  ifeq ($(wildcard .hg),.hg)
  $(info Building with erg...)
  include Makefile.dldc
  include $(shell opam var erg:lib)/Makefile.erg
  else
  $(info Bootstrapping with ocaml-makefile...)
  OCAMLMAKEFILE = $(shell opam var ocaml-makefile:lib)/OCamlMakefile
  include $(OCAMLMAKEFILE)
  endif
endif
