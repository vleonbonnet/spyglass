#!/bin/bash
# gui-emacs.sh - run a graphical Emacs for tests, out of the user's way.
#
# Usage: test/gui-emacs.sh [-t SECONDS] [-m KB] EMACS ARGUMENT...
#
# Runs EMACS, a graphical build, as `EMACS -Q --chdir "$PWD" -l
# gui-emacs.el ARGUMENT...', in the environment given here.  The
# arguments must end with `kill-emacs'.  Prints what that Emacs wrote
# to its standard output and error once it exits, and exits with its
# status: 124 when it ran past SECONDS (600) and was killed, 137 when
# its resident memory passed KB (2097152, 2 GB).
#
# Its frames show nowhere and never take the focus.  On macOS it starts
# through LaunchServices, neither activated nor shown (`open -g -j -n
# -W'), as a new instance of the application holding EMACS, whatever
# instance the user runs; then gui-emacs.el moves its frames off screen,
# fully transparent and refusing focus, before they show.  `open'
# returns neither the status nor the output of the application, nor
# does a timeout around it reach the application: gui-emacs.el writes
# its process ID and the status `kill-emacs' is given to a directory of
# the run, and the application's output goes to files there.
#
# Never start a graphical Emacs by other means for tests: started
# directly, the application comes in front of the user's windows and
# takes the keyboard.

set -u

usage() {
  echo "Usage: $0 [-t SECONDS] [-m KB] EMACS ARGUMENT..." >&2
  exit 2
}

limit=600
rss_limit=2097152
while getopts t:m: option; do
  case $option in
    t) limit=$OPTARG ;;
    m) rss_limit=$OPTARG ;;
    *) usage ;;
  esac
done
shift $((OPTIND - 1))
[ $# -ge 1 ] || usage
emacs=$1
shift

here=$(cd "$(dirname "$0")" && pwd)
run=$(mktemp -d "${TMPDIR:-/tmp}/gui-emacs.XXXXXX")
# Loaded from the run's directory, which names this Emacs's processes.
cp "$here/gui-emacs.el" "$run/"
args=(-Q --chdir "$(pwd)" -l "$run/gui-emacs.el" "$@")

# The application bundle holding EMACS, on macOS.
app=
if [ "$(uname)" = Darwin ]; then
  directory=$("$emacs" -Q --batch --eval '(princ invocation-directory)' 2> /dev/null)
  case $directory in
    */Contents/MacOS/) app=${directory%/Contents/MacOS/} ;;
  esac
  if [ -z "$app" ] || [ ! -d "$app" ]; then
    echo "gui-emacs: $emacs is not in an application bundle" >&2
    rm -rf "$run"
    exit 2
  fi
fi

if [ -n "$app" ]; then
  # The application gets the environment of launchd, unless told.
  # SHLVL keeps the Mac port from starting again through a login shell.
  environment=(--env SHLVL=1)
  while IFS= read -r name; do
    case $name in
      _ | PWD | OLDPWD | SHLVL | XPC_* | __CF*) continue ;;
    esac
    value=${!name-}
    case $value in
      *$'\n'*) continue ;;
    esac
    environment+=(--env "$name=$value")
  done < <(compgen -e)
  # Preferences on the command line go last: GNU Emacs's Cocoa build
  # stops processing options at them.  This one keeps macOS from
  # restoring windows, or asking to.
  open -g -j -n -W --stdin /dev/null --stdout "$run/out" --stderr "$run/err" \
    "${environment[@]}" -a "$app" --args "${args[@]}" -ApplePersistenceIgnoreState YES &
else
  # Elsewhere, a virtual display when there is one.
  wrap=()
  if [ "$(uname)" = Linux ] && command -v xvfb-run > /dev/null; then
    wrap=(xvfb-run -a)
  fi
  ${wrap[@]+"${wrap[@]}"} "$emacs" "${args[@]}" < /dev/null > "$run/out" 2> "$run/err" &
fi
launcher=$!

# This Emacs's processes: the one gui-emacs.el told, else any whose
# command line names the run.
emacs_pids() {
  if [ -s "$run/pid" ]; then
    cat "$run/pid"
  else
    for pid in $(pgrep -f -- "$run/gui-emacs.el"); do
      [ "$pid" != "$launcher" ] && ps -o comm= -p "$pid" | grep -qi emacs && echo "$pid"
    done
  fi
}

stop() {
  local pid
  for pid in $(emacs_pids); do
    kill "$pid" 2> /dev/null
  done
  for _ in 1 2 3 4 5 6 7 8 9 10; do
    kill -0 "$launcher" 2> /dev/null || break
    sleep 0.5
  done
  for pid in $(emacs_pids); do
    kill -9 "$pid" 2> /dev/null
  done
  kill "$launcher" 2> /dev/null
}

trap 'stop; rm -rf "$run"; exit 130' INT TERM

status=
start=$SECONDS
while kill -0 "$launcher" 2> /dev/null; do
  if [ $((SECONDS - start)) -ge "$limit" ]; then
    echo "gui-emacs: still running after $limit s: killed" >&2
    status=124
    stop
    break
  fi
  for pid in $(emacs_pids); do
    rss=$(ps -o rss= -p "$pid" 2> /dev/null | tr -d ' ')
    if [ -n "$rss" ] && [ "$rss" -gt "$rss_limit" ]; then
      echo "gui-emacs: resident memory past $rss_limit kB: killed" >&2
      status=137
    fi
  done
  if [ -n "$status" ]; then
    stop
    break
  fi
  sleep 0.5
done
wait "$launcher"
exited=$?

if [ -z "$status" ]; then
  if [ -s "$run/status" ] && grep -qx '[0-9]*' "$run/status"; then
    status=$(cat "$run/status")
  elif [ -z "$app" ]; then
    status=$exited
  else
    echo "gui-emacs: Emacs exited without telling its status" >&2
    status=1
  fi
fi
cat "$run/out" 2> /dev/null
# macOS says so whenever an application starts with the preference.
grep -v 'ApplePersistenceIgnoreState: Existing state will not be touched' "$run/err" >&2 2> /dev/null
rm -rf "$run"
exit "$status"
