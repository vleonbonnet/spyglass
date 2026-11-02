# AGENTS.md - spyglass

Emacs Lisp library mirroring live buffers between two Emacs instances, on one host or across SSH: an owner shares buffers, a viewer opens read-only mirrors that follow every change (text, faces, display properties, overlays, the owner's cursor) and sends the keys typed in them back. Nothing in it knows about agents or any front end; drover and others build on its API.

## Rules

- Never evaluate code in the user's running Emacs to test this library (no `emacsclient`, no Emacs MCP evaluation). Every test and benchmark runs in separate `emacs -Q --batch` processes started by `make`, except `make test-gui`, which starts a graphical `emacs -Q` of its own, iconified.
- Memory safety is mandatory. Every batch Emacs runs under `timeout` (600 s at most, `TIMEOUT` in the Makefile). Benchmarks size their buffers explicitly, never grow data without bound, and every benchmark Emacs kills itself past 2 GB of resident memory (`spyglass-bench-rss-limit`); watch `ps -o rss= -p PID` when running anything heavier by hand. A guard on this machine kills batch Emacs above 8 GB; never get near it.
- A graphical Emacs (`make test-gui`, or by hand) runs under `timeout` too, with the same memory guard, `-Q`, native compilation off (it would write to the user's cache), started with `--chdir` (a macOS application starts in the home directory) and, on macOS, `-ApplePersistenceIgnoreState YES` last: GNU Emacs's Cocoa build stops processing options at it. It runs its tests from a timer once the window system's loop runs, and writes their results to a file before exiting. Pick processes to inspect by their full command line: the user's Emacs runs the same binary.
- Batch Emacs keeps SIGPIPE's default action (`sysdep.c`: only interactive Emacs ignores it), so writing to a peer that just closed kills it. The Makefile runs every batch Emacs after `trap '' PIPE`; do the same by hand.
- On another host (Core, Windows, reached with `myssh core`, never `ssh`), work only in a temporary directory you create, run only batch Emacs (`/ucrt64/bin/emacs -Q --batch`) under `timeout`, never touch the running Emacs or files outside that directory, and remove the directory when done. `make bench BENCH_REMOTE=core BENCH_SSH=myssh BENCH_REMOTE_EMACS=/ucrt64/bin/emacs` does all of that.
- `make compile` must stay free of warnings and `make lint` (checkdoc) clean. `make test` must pass; it uses nothing beyond 127.0.0.1.
- Nothing a peer sends is ever given to `read` or `eval`. Headers are JSON, parsed with `json-parse-string`; property values are decoded as data (`spyglass-wire-decode`) and sanitized (`spyglass-wire-sanitize-props`) on the viewer, whatever the owner did. Keep the sanitizers strict: a new display form or face attribute is allowed only if redisplay cannot evaluate anything from it. Numbers a peer sends that size anything, as window sizes, are bounded on the receiving end (`spyglass-max-terminal-size`).
- A new optional message is announced in `spyglass-wire-capabilities` and sent only to peers announcing it (`spyglass-connection-capable-p`), so that older peers of the same protocol version keep working; a change every peer must understand changes `spyglass-wire-protocol`.
- Parking frames never become visible, selected between commands, or focused, and stay out of frame hooks, `next-frame`, desktops and tab bars; the user's own windows are never resized. Code walking frames tells parking frames by `spyglass-parking-frame-p`.
- Never signal into a process filter, sentinel or timer: wrap their bodies in `spyglass--guard`, report errors through `spyglass--report`.
- The owner never queues updates: it keeps per viewer the ranges not yet sent, and reads their text when sending. Anything per viewer must stay bounded by `spyglass-max-intervals`, `spyglass-window` and `spyglass-max-styles`.
- Library functions never prompt; only interactive commands in spyglass.el may.
- Byte code must not depend on optional packages (Eat, Ghostel, vterm): adapters declare what they use and are loaded by `with-eval-after-load` in spyglass.el.
- Comments are sparse and explain why, not what. Docstrings are complete sentences; the first line stands alone.
- Commit messages follow drover's: an imperative subject, then prose paragraphs saying what changed and why.

## Verification

- `make compile lint test` before every commit. `EAT_EL`, `VTERM_EL` (vterm.el with its module built beside it, in a copy of the sources), `GHOSTEL_EL` and `GHOSTEL_MODULE_DIR` add tests and benchmarks of real terminals.
- `make test-gui` (with `GHOSTEL_EL` and `GHOSTEL_MODULE_DIR`, and `EMACS` a graphical build) for anything touching parking frames or windows; on macOS run it with the Mac port and with GNU Emacs's Cocoa build when you can.
- `make bench` for performance work (localhost), and with `BENCH_REMOTE` across hosts. A scenario's `exact` column must say `yes`: the owner's and the mirror's digests agree.

## Layout

See `doc/design.org` for the module map, the protocol and the main design choices, and `README.org` for the API.
