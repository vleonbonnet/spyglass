# Build and test spyglass in separate batch Emacs processes.
#
# Every batch Emacs runs under `timeout': none may outlive TIMEOUT
# seconds.  The benchmarks also watch the memory of every Emacs they
# start (see test/spyglass-bench.el).
#
# EAT_EL is a directory holding eat.el, for the tests and benchmarks of
# the Eat adapter; without it they are skipped.  GHOSTEL_EL and
# GHOSTEL_MODULE_DIR locate Ghostel's Lisp and its native module;
# VTERM_EL a directory holding vterm.el and its built module.  BENCH_REMOTE names an SSH host to run the
# benchmarks against too, through BENCH_SSH.

EMACS ?= emacs
TIMEOUT ?= 600
DEPS ?= $(HOME)/.emacs.d/elpaca/builds
EAT_EL ?=
GHOSTEL_EL ?=
GHOSTEL_MODULE_DIR ?=
VTERM_EL ?=
BENCH_REMOTE ?=
BENCH_SSH ?= ssh
BENCH_REMOTE_EMACS ?= emacs
BENCH_ONLY ?=

# Stale byte code must never shadow an edited source, in builds or tests.
PREFER_NEWER = --eval '(setq load-prefer-newer t)'
# A batch Emacs keeps SIGPIPE's default action, which kills it when it
# writes to a peer that just closed; an interactive one ignores it.
# Ignored signals stay ignored across exec.
NOPIPE = trap '' PIPE;
EMACS_BATCH = timeout $(TIMEOUT) $(EMACS) -Q --batch -L . -L test $(PREFER_NEWER)
BATCH = $(NOPIPE) $(EMACS_BATCH)
NO_WARNINGS = --eval '(setq byte-compile-error-on-warn t)'

SOURCES = $(filter-out %-pkg.el %-autoloads.el,$(wildcard spyglass*.el))
TESTS = $(wildcard test/*-test.el)

.PHONY: all compile test lint bench clean

all: compile test

compile:
	$(BATCH) $(NO_WARNINGS) -f batch-byte-compile $(SOURCES)

test:
	$(NOPIPE) SPYGLASS_EAT_EL="$(EAT_EL)" SPYGLASS_VTERM_EL="$(VTERM_EL)" \
	SPYGLASS_GHOSTEL_EL="$(GHOSTEL_EL)" SPYGLASS_GHOSTEL_MODULE_DIR="$(GHOSTEL_MODULE_DIR)" \
	$(EMACS_BATCH) -l test/spyglass-test-helpers.el \
	  $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) --eval '(dolist (file (list $(patsubst %,"%",$(SOURCES)))) (checkdoc-file file))'

# Not part of test: measures two Emacs processes mirroring through
# 127.0.0.1, and through SSH to BENCH_REMOTE when set.  See
# test/spyglass-bench.el.
bench: compile
	$(NOPIPE) SPYGLASS_EMACS="$(EMACS)" SPYGLASS_TIMEOUT="$(TIMEOUT)" \
	SPYGLASS_EAT_EL="$(EAT_EL)" SPYGLASS_GHOSTEL_EL="$(GHOSTEL_EL)" \
	SPYGLASS_GHOSTEL_MODULE_DIR="$(GHOSTEL_MODULE_DIR)" \
	SPYGLASS_VTERM_EL="$(VTERM_EL)" \
	SPYGLASS_BENCH_REMOTE="$(BENCH_REMOTE)" SPYGLASS_BENCH_SSH="$(BENCH_SSH)" \
	SPYGLASS_BENCH_REMOTE_EMACS="$(BENCH_REMOTE_EMACS)" SPYGLASS_BENCH_ONLY="$(BENCH_ONLY)" \
	$(EMACS_BATCH) -l test/spyglass-bench.el -f spyglass-bench-main

clean:
	rm -f *.elc test/*.elc
