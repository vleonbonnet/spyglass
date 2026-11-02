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
# benchmarks against too, through BENCH_SSH.  LAG_SSH_HOST names a host
# reached through LAG_SSH without a prompt and sharing this file system,
# as localhost, for the SSH scenarios of lag.

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
BENCH_WINDOW ?=
LAG_SSH ?= ssh
LAG_SSH_HOST ?=

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
# The GUI tests run in a graphical Emacs: see test-gui.
TESTS = $(filter-out test/spyglass-gui-test.el,$(wildcard test/*-test.el))

# A graphical Emacs for test-gui, started iconified.  macOS restores
# no saved windows into it; the option goes last, since GNU Emacs's
# Cocoa build stops processing options at it, as unknown.
GUI_EMACS ?= $(EMACS)
GUI_FLAGS ?= $(if $(filter Darwin,$(shell uname)),-ApplePersistenceIgnoreState YES)
GUI_SELECTOR ?=

.PHONY: all compile test test-gui lint bench lag lag-gui clean

all: compile test

compile:
	$(BATCH) $(NO_WARNINGS) -f batch-byte-compile $(SOURCES)

# Each test file runs in a batch Emacs of its own: memory a file's large
# frames and buffers leave to Emacs's allocator stays with it, and all
# of them in one Emacs went past the 2 GB a batch Emacs may take.
test:
	@status=0; \
	for file in $(TESTS); do \
	  $(NOPIPE) SPYGLASS_EAT_EL="$(EAT_EL)" SPYGLASS_VTERM_EL="$(VTERM_EL)" \
	  SPYGLASS_GHOSTEL_EL="$(GHOSTEL_EL)" SPYGLASS_GHOSTEL_MODULE_DIR="$(GHOSTEL_MODULE_DIR)" \
	  $(EMACS_BATCH) -l test/spyglass-test-helpers.el -l $$file -f ert-run-tests-batch-and-exit \
	    < /dev/null || { echo "$$file failed"; status=1; }; \
	done; \
	exit $$status

lint:
	$(BATCH) --eval '(dolist (file (list $(patsubst %,"%",$(SOURCES)))) (checkdoc-file file))'

# Parking frames need a graphical display.  The graphical Emacs writes
# the results to a temporary file, printed here; the benchmark's agent,
# a batch Emacs, mirrors a Ghostel terminal from it.  The tests start
# from a timer, once the window system's event loop runs.  Native
# compilation stays off: it would write to the user's cache.
test-gui: compile
	@$(NOPIPE) log=$$(mktemp); \
	SPYGLASS_GUI_LOG=$$log SPYGLASS_GUI_SELECTOR="$(GUI_SELECTOR)" \
	SPYGLASS_EMACS="$(EMACS)" SPYGLASS_TIMEOUT="$(TIMEOUT)" \
	SPYGLASS_GHOSTEL_EL="$(GHOSTEL_EL)" SPYGLASS_GHOSTEL_MODULE_DIR="$(GHOSTEL_MODULE_DIR)" \
	timeout $(TIMEOUT) $(GUI_EMACS) -Q -iconic --chdir "$(CURDIR)" \
	  --eval '(setq native-comp-jit-compilation nil load-prefer-newer t)' \
	  -L . -L test \
	  --eval '(condition-case err (progn (load "spyglass-gui-test") (run-with-timer 0.5 nil (function spyglass-gui-test-run))) (error (write-region (format "%S\n" err) nil (getenv "SPYGLASS_GUI_LOG")) (kill-emacs 2)))' \
	  $(GUI_FLAGS); \
	status=$$?; cat $$log; rm -f $$log; exit $$status

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
	SPYGLASS_BENCH_WINDOW="$(BENCH_WINDOW)" \
	$(EMACS_BATCH) -l test/spyglass-bench.el -f spyglass-bench-main

# Not part of test: how long attempts to connect, failing or not, keep
# Emacs from running timers.  See test/spyglass-lag.el.
lag: compile
	$(NOPIPE) SPYGLASS_LAG_SSH="$(LAG_SSH)" SPYGLASS_LAG_SSH_HOST="$(LAG_SSH_HOST)" \
	$(EMACS_BATCH) -l test/spyglass-lag.el -f spyglass-lag-main

# The same in a graphical Emacs, started iconified as for test-gui,
# waiting in its command loop as the user's does.
lag-gui: compile
	@$(NOPIPE) log=$$(mktemp); \
	SPYGLASS_LAG_LOG=$$log SPYGLASS_LAG_SSH="$(LAG_SSH)" SPYGLASS_LAG_SSH_HOST="$(LAG_SSH_HOST)" \
	timeout $(TIMEOUT) $(GUI_EMACS) -Q -iconic --chdir "$(CURDIR)" \
	  --eval '(setq native-comp-jit-compilation nil load-prefer-newer t)' \
	  -L . -L test \
	  --eval '(condition-case err (progn (load "spyglass-lag") (run-with-timer 0.5 nil (function spyglass-lag-main))) (error (write-region (format "%S\n" err) nil (getenv "SPYGLASS_LAG_LOG")) (kill-emacs 2)))' \
	  $(GUI_FLAGS); \
	status=$$?; cat $$log; rm -f $$log; exit $$status

clean:
	rm -f *.elc test/*.elc
