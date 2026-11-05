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
XFER_DIRS ?=
XFER_PATHS ?=
XFER_SIZES ?=
XFER_WINDOWS ?=
XFER_CHUNKS ?=
XFER_READ_MAX ?=
XFER_PIPE ?=
XFER_PIECE ?=
XFER_FLIGHT ?=
XFER_REPS ?=
XFER_BACK_SSH ?=
XFER_BACK_ARGS ?=
XFER_BACK_HOST ?=
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
# One test file for test-one, and the tests of it to run, a regexp.
FILE ?=
SELECTOR ?=

# A graphical Emacs for test-gui and lag-gui, started by
# test/gui-emacs.sh: on macOS through `open', neither activated nor
# shown, its frames off screen and transparent, so that nothing comes in
# front of the user's windows nor takes the focus; under the same
# timeout and memory guard as batch Emacs, its status and output
# returned.  Never start one otherwise.
GUI_EMACS ?= $(EMACS)
GUI = test/gui-emacs.sh -t $(TIMEOUT) $(GUI_EMACS)
# The tests of test-gui to run, a regexp; empty for every GUI test.
GUI_SELECTOR ?=

.PHONY: all compile test-compile test test-one test-gui lint bench transfer lag lag-gui clean

all: compile test

compile:
	$(BATCH) $(NO_WARNINGS) -f batch-byte-compile $(SOURCES)

# The byte code the tests load, as a package manager installs it,
# compiled again once a source changed, spyglass's or one of the
# terminals given, whose macros expand into it; byte code whose source
# is gone is removed.  Warnings are for compile to report.
TEST_DEPENDS = $(foreach dir,$(EAT_EL) $(GHOSTEL_EL) $(VTERM_EL),$(wildcard $(dir)/*.el))
test-compile:
	@for elc in spyglass*.elc; do [ ! -f "$$elc" ] || [ -f "$${elc%c}" ] || rm -f "$$elc"; done; \
	stale=; for elc in $(SOURCES:.el=.elc); do [ -f "$$elc" ] || stale=1; done; \
	if [ -z "$$stale" ]; then \
	  oldest=$$(ls -tr $(SOURCES:.el=.elc) | head -1); \
	  for el in $(SOURCES) $(TEST_DEPENDS); do [ "$$el" -nt "$$oldest" ] && stale=1 && break; done; \
	fi; \
	if [ -n "$$stale" ]; then \
	  log=$$(mktemp); \
	  if $(BATCH) -f batch-byte-compile $(SOURCES) > "$$log" 2>&1; then \
	    echo "Compiled $(words $(SOURCES)) files for the tests"; rm -f "$$log"; \
	  else \
	    cat "$$log"; rm -f "$$log"; exit 1; \
	  fi; \
	fi

# Each test file runs in a batch Emacs of its own: memory a file's large
# frames and buffers leave to Emacs's allocator stays with it, and all
# of them in one Emacs went past the 2 GB a batch Emacs may take.  JOBS
# of them run at once, through test/batch-tests.sh: each under TIMEOUT
# seconds and RSS_LIMIT kB of resident memory, its standard input
# closed so that a test reading it fails rather than waits.  A line
# tells each Emacs's count as it ends, then the log of each that failed
# shows.  TEST_SHARDS names the longest files, longest first, and how
# many Emacs share the tests of each, which start first: shares that
# take about as long as one another by the times of the last run.
# SELECTOR, a regexp, runs some tests only.  TEST_CACHE keeps between
# runs those times and the trampolines native compilation makes for
# primitives tests redefine (test/batch-tests.el).
JOBS ?= $(shell n=$$(getconf _NPROCESSORS_ONLN 2> /dev/null || echo 4); [ "$$n" -gt 8 ] && n=8; echo $$n)
RSS_LIMIT ?= 2097152
TEST_CACHE ?= $(patsubst %/,%,$(or $(TMPDIR),$(TEMP),/tmp))/spyglass-tests
TEST_SHARDS = spyglass-conn-test:3 spyglass-e2e-test:2 spyglass-transport-test:2 spyglass-paced-test:2
shards = $(or $(word 2,$(subst :, ,$(filter $(1):%,$(TEST_SHARDS)))),1)
SHARDED = $(foreach shard,$(TEST_SHARDS),test/$(word 1,$(subst :, ,$(shard))).el)
TEST_JOBS = $(foreach test,$(filter $(TESTS),$(SHARDED)) $(filter-out $(SHARDED),$(TESTS)),\
  $(foreach n,$(call shards,$(basename $(notdir $(test)))),\
    $(foreach i,$(wordlist 1,$(n),1 2 3 4 5 6 7 8 9),$(test).$(i).$(n))))
TEST_LOGS ?=
TEST_ENV = SPYGLASS_EAT_EL="$(EAT_EL)" SPYGLASS_VTERM_EL="$(VTERM_EL)" \
  SPYGLASS_GHOSTEL_EL="$(GHOSTEL_EL)" SPYGLASS_GHOSTEL_MODULE_DIR="$(GHOSTEL_MODULE_DIR)" \
  SELECTOR="$(SELECTOR)" BATCH_TESTS_ELN="$(TEST_CACHE)/eln"

test: test-compile
	@logs=$$(mktemp -d "$${TMPDIR:-/tmp}/spyglass-tests.XXXXXX"); start=$$(date +%s); \
	mkdir "$$logs/times"; cp "$(TEST_CACHE)"/times/*.times "$$logs/times" 2> /dev/null; \
	$(MAKE) --no-print-directory -j$(JOBS) TEST_LOGS="$$logs" $(addprefix test-job/,$(TEST_JOBS)); \
	test/batch-tests.sh report "$$logs" $(JOBS) $$(($$(date +%s) - start)) "$(TEST_CACHE)/times"

# One Emacs of test: test-job/FILE.I.N runs the I-th of N shares of
# the tests of FILE, by the times of the run before this one.
test-job/%:
	@job=$*; n=$${job##*.}; job=$${job%.*}; i=$${job##*.}; file=$${job%.*}; \
	base=$$(basename $$file .el); name=$$base; [ $$n -eq 1 ] || name=$$base.$$i; \
	$(TEST_ENV) BATCH_TESTS_FILE=$$file BATCH_TESTS_SHARD=$$i/$$n \
	BATCH_TESTS_TIMES="$(TEST_LOGS)/times/$$base.times" BATCH_TESTS_TIMES_OUT="$(TEST_LOGS)/$$name.times" \
	test/batch-tests.sh run -t $(TIMEOUT) -m $(RSS_LIMIT) -o "$(TEST_LOGS)/$$name.log" -n $$name \
	  $(EMACS) -Q --batch -L . -L test $(PREFER_NEWER) \
	  -l test/spyglass-test-helpers.el -l $$file -l test/batch-tests.el -f batch-tests-run

# One test file, FILE, or the tests of it SELECTOR matches: what to run
# while working, before the whole of test.
test-one: test-compile
	@if [ -z "$(FILE)" ]; then echo "test-one needs FILE, a test file"; exit 2; fi
	$(NOPIPE) $(TEST_ENV) BATCH_TESTS_FILE="$(FILE)" \
	$(EMACS_BATCH) -l test/spyglass-test-helpers.el -l $(FILE) -l test/batch-tests.el -f batch-tests-run \
	  < /dev/null

lint:
	$(BATCH) --eval '(dolist (file (list $(patsubst %,"%",$(SOURCES)))) (checkdoc-file file))'

# Parking frames need a graphical display.  The graphical Emacs writes
# the results to a temporary file, printed here; the benchmark's agent,
# a batch Emacs, mirrors a Ghostel terminal from it.  The tests start
# from a timer, once the window system's event loop runs.
test-gui: test-compile
	@$(NOPIPE) log=$$(mktemp); \
	SPYGLASS_GUI_LOG=$$log SPYGLASS_GUI_SELECTOR="$(GUI_SELECTOR)" \
	SPYGLASS_EMACS="$(EMACS)" SPYGLASS_TIMEOUT="$(TIMEOUT)" \
	SPYGLASS_GHOSTEL_EL="$(GHOSTEL_EL)" SPYGLASS_GHOSTEL_MODULE_DIR="$(GHOSTEL_MODULE_DIR)" \
	$(GUI) $(PREFER_NEWER) -L . -L test \
	  --eval '(condition-case err (progn (load "spyglass-gui-test") (run-with-timer 0.5 nil (function spyglass-gui-test-run))) (error (write-region (format "%S\n" err) nil (getenv "SPYGLASS_GUI_LOG")) (kill-emacs 2)))'; \
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

# Not part of test: the benchmark's agent takes payloads from this
# Emacs as drover's clones do, over a sweep of windows, chunks and
# reads, through 127.0.0.1 or through SSH to BENCH_REMOTE, both ways
# of dialing (XFER_PATHS).  See test/spyglass-transfer.el.
transfer: compile
	$(NOPIPE) SPYGLASS_EMACS="$(EMACS)" SPYGLASS_TIMEOUT="$(TIMEOUT)" \
	SPYGLASS_BENCH_REMOTE="$(BENCH_REMOTE)" SPYGLASS_BENCH_SSH="$(BENCH_SSH)" \
	SPYGLASS_BENCH_REMOTE_EMACS="$(BENCH_REMOTE_EMACS)" \
	XFER_DIRS="$(XFER_DIRS)" XFER_PATHS="$(XFER_PATHS)" XFER_SIZES="$(XFER_SIZES)" XFER_WINDOWS="$(XFER_WINDOWS)" \
	XFER_CHUNKS="$(XFER_CHUNKS)" XFER_READ_MAX="$(XFER_READ_MAX)" XFER_PIPE="$(XFER_PIPE)" \
	XFER_PIECE="$(XFER_PIECE)" XFER_FLIGHT="$(XFER_FLIGHT)" XFER_REPS="$(XFER_REPS)" \
	XFER_BACK_SSH="$(XFER_BACK_SSH)" XFER_BACK_ARGS="$(XFER_BACK_ARGS)" \
	XFER_BACK_HOST="$(XFER_BACK_HOST)" \
	$(EMACS_BATCH) -l test/spyglass-transfer.el -f spyglass-transfer-main

# Not part of test: how long attempts to connect, failing or not, keep
# Emacs from running timers.  See test/spyglass-lag.el.
lag: compile
	$(NOPIPE) SPYGLASS_LAG_SSH="$(LAG_SSH)" SPYGLASS_LAG_SSH_HOST="$(LAG_SSH_HOST)" \
	$(EMACS_BATCH) -l test/spyglass-lag.el -f spyglass-lag-main

# The same in a graphical Emacs, started as for test-gui, waiting in its
# command loop as the user's does.
lag-gui: compile
	@$(NOPIPE) log=$$(mktemp); \
	SPYGLASS_LAG_LOG=$$log SPYGLASS_LAG_SSH="$(LAG_SSH)" SPYGLASS_LAG_SSH_HOST="$(LAG_SSH_HOST)" \
	$(GUI) $(PREFER_NEWER) -L . -L test \
	  --eval '(condition-case err (progn (load "spyglass-lag") (run-with-timer 0.5 nil (function spyglass-lag-main))) (error (write-region (format "%S\n" err) nil (getenv "SPYGLASS_LAG_LOG")) (kill-emacs 2)))'; \
	status=$$?; cat $$log; rm -f $$log; exit $$status

clean:
	rm -f *.elc test/*.elc
