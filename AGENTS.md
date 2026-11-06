# AGENTS.md - spyglass

Emacs Lisp library mirroring live buffers between two Emacs instances, on one host or across SSH: an owner shares buffers, a viewer opens read-only mirrors that follow every change and sends the keys typed in them back. It knows nothing of agents or front ends; drover builds on its API (`README.org`). The module map, the protocol and the design choices are in `doc/design.org`.

## Commands

- `make test-one FILE=test/spyglass-conn-test.el` (`SELECTOR=regexp` for some of its tests): the file you change, in seconds. Use it while working.
- `make compile lint test`: once before every commit. `compile` must stay free of warnings and `lint` (checkdoc) clean; `test` takes about 10 s, each file in a batch Emacs of its own, so no test may count on another having run. `EAT_EL`, `VTERM_EL`, `GHOSTEL_EL` and `GHOSTEL_MODULE_DIR` add the tests of real terminals.
- `make test-gui` (about 17 s, one hidden graphical Emacs; `GUI_SELECTOR=regexp`) only when a change touches parking frames or what windows show, once before merging. `make lag-gui` only for how attempts to connect wait.
- `make bench` for performance work, across hosts with `BENCH_REMOTE=core BENCH_SSH=myssh BENCH_REMOTE_EMACS=/ucrt64/bin/emacs`; its `exact` column must say `yes`. `make transfer` sweeps channel windows and chunks; size it to stay under `TIMEOUT`.

## The user's Emacs

You may evaluate code in the user's running Emacs through Anvil, to inspect state or to try a change; follow `~/.emacs.d/AGENTS.md` (no buffers displayed, no prompts).

- This Mac installs spyglass from this checkout. After merging to main: `(val/elpaca/update-packages 'spyglass :rebuild t)` with `emacs-eval-async`.
- Core, the Windows host, installs it from GitHub: once main is pushed, `myssh core "/ucrt64/bin/emacsclient -f ~/.emacs.d/server/server --eval \"(val/elpaca/update-packages 'spyglass)\""`.
- Both run in the background. The outcome, or the failure: `(list (car (gethash (val/elpaca/package-key 'spyglass) val/elpaca/stories)) (val/maintenance/failure 'update (val/elpaca/package-key 'spyglass)))`.

## Boundaries

- Never start a graphical Emacs yourself (`emacs` without `--batch`, `Emacs.app`, `open -a Emacs`): on macOS it takes the user's screen and keyboard. Only `make test-gui` and `make lag-gui` start one, hidden, through `test/gui-emacs.sh`; an experiment needing a graphical frame becomes a test of `test/spyglass-gui-test.el`. Pick processes to inspect by their full command line: the user's Emacs runs the same binary.
- Memory: every batch Emacs runs under `timeout` (`TIMEOUT`, 600 s) and the benchmarks kill themselves past 2 GB; a guard on this Mac kills batch Emacs above 8 GB. Size benchmark data explicitly and watch `ps -o rss= -p PID` when running anything heavy by hand.
- Batch Emacs dies of SIGPIPE when it writes to a peer that just closed: the Makefile runs every batch Emacs after `trap '' PIPE`; do the same by hand.
- Tests and experiments on Core run batch Emacs (`/ucrt64/bin/emacs -Q --batch`) under `timeout` in a temporary directory you remove after.

## Conventions the code does not show

- Nothing a peer sends is given to `read` or `eval`: headers and channel messages are JSON (`json-parse-string`), property values are decoded as data (`spyglass-wire-decode`) and sanitized (`spyglass-wire-sanitize-props`). A display form or face attribute is allowed only if redisplay cannot evaluate anything from it.
- A new optional message is announced in `spyglass-wire-capabilities` and sent only to peers announcing it (`spyglass-connection-capable-p`); a change every peer must understand changes `spyglass-wire-protocol`.
- Frames are written by `spyglass--send-frame` or `spyglass--send` only, never `process-send-string`: a waiting write runs timers and filters, which would interleave their frames. Frames are dispatched only through `spyglass--handle`.
- Process filters, sentinels and timers run their body in `spyglass--guard` and report through `spyglass--report`.
- What spyglass keeps reaches back to what holds it only through `spyglass--ref`; `test/spyglass-print-test.el` checks it.
- Library functions never prompt; only the interactive commands of `spyglass.el` may.
- Comments are sparse and explain why. Docstrings are complete sentences; the first line stands alone.

## Finishing

Commit messages: an imperative subject, then prose paragraphs saying what changed and why. Merge onto the local main, run `make compile lint test` there, push main, then update spyglass on this Mac and on Core as above.
