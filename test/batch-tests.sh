#!/bin/bash
# batch-tests.sh - run test files in batch Emacs, several at once, bounded.
#
# Usage: test/batch-tests.sh run [-t SECONDS] [-m KB] -o LOG -n NAME COMMAND...
#        test/batch-tests.sh report DIRECTORY JOBS SECONDS [TIMES]
#
# The Makefile's test target runs one `run' per test file, or per
# shard of a file (batch-tests.el), as many at once as `make -j' allows,
# each logging to DIRECTORY, then `report'.
#
# `run' runs COMMAND, a batch Emacs, with its standard input closed and
# SIGPIPE ignored, its output going to LOG.  It is killed once it runs
# past SECONDS (600), or once its resident memory passes KB (2097152,
# 2 GB), as test/gui-emacs.sh does a graphical Emacs; its status, 124
# and 137 then, goes to LOG.status.  It prints one line, NAME and the
# count ERT gave, and exits 0 whatever happened: `report' tells.
#
# `report' prints the log of each Emacs that failed, the tests that
# passed left out, then the counts of the run, which took SECONDS with
# JOBS Emacs at most at once; it exits 1 when an Emacs failed, and
# removes DIRECTORY otherwise.  The times of tests the Emacs wrote to
# DIRECTORY, NAME.times or NAME.I.times for the I-th share of a file,
# go to TIMES/NAME.times, replacing those of the same tests there:
# batch-tests.el shares a file's tests by them.

set -u

usage() {
  echo "Usage: $0 run [-t SECONDS] [-m KB] -o LOG -n NAME COMMAND..." >&2
  echo "       $0 report DIRECTORY JOBS SECONDS [TIMES]" >&2
  exit 2
}

# ERT's summary, "Ran N tests, ... (DATE, SECONDS sec)", without the date.
ran() {
  grep '^Ran [0-9]* test' "$1" 2> /dev/null | tail -1 \
    | sed -E 's/ \([^,]*, ([0-9.]+) sec\)$/ (\1 s)/'
}

run() {
  local limit=600 rss_limit=2097152 log='' name='' option OPTIND=1
  while getopts t:m:o:n: option; do
    case $option in
      t) limit=$OPTARG ;;
      m) rss_limit=$OPTARG ;;
      o) log=$OPTARG ;;
      n) name=$OPTARG ;;
      *) usage ;;
    esac
  done
  shift $((OPTIND - 1))
  [ $# -ge 1 ] && [ -n "$log" ] || usage
  [ -n "$name" ] || name=$log
  rm -f "$log.status" "$log.killed"
  # A batch Emacs keeps SIGPIPE's default action, which kills it when it
  # writes to a peer that just closed; ignored signals stay ignored
  # across exec.
  trap '' PIPE
  "$@" < /dev/null > "$log" 2>&1 &
  local pid=$!
  (
    start=$SECONDS
    while kill -0 "$pid" 2> /dev/null; do
      reason=
      if [ $((SECONDS - start)) -ge "$limit" ]; then
        reason="still running after $limit s"
        echo 124 > "$log.killed"
      else
        rss=$(ps -o rss= -p "$pid" 2> /dev/null | tr -d ' ')
        if [ -n "$rss" ] && [ "$rss" -gt "$rss_limit" ] 2> /dev/null; then
          reason="resident memory past $rss_limit kB"
          echo 137 > "$log.killed"
        fi
      fi
      if [ -n "$reason" ]; then
        echo "batch-tests: $reason: killed" >> "$log"
        # Emacs ends the processes it started as it exits on SIGTERM.
        kill "$pid" 2> /dev/null
        for _ in 1 2 3 4 5 6 7 8 9 10; do
          kill -0 "$pid" 2> /dev/null || break
          sleep 0.2
        done
        kill -9 "$pid" 2> /dev/null
        break
      fi
      sleep 0.5
    done
  ) &
  local watcher=$!
  wait "$pid"
  local status=$?
  kill "$watcher" 2> /dev/null
  wait "$watcher" 2> /dev/null
  if [ -s "$log.killed" ]; then
    status=$(cat "$log.killed")
  fi
  rm -f "$log.killed"
  echo "$status" > "$log.status"
  local summary
  summary=$(ran "$log")
  if [ "$status" -eq 0 ]; then
    echo "$name: ${summary:-no test ran}"
  else
    echo "$name: FAILED, status $status: ${summary:-no count}"
  fi
  return 0
}

# Merge the times of tests in DIRECTORY into those of the directory TIMES.
keep_times() {
  local directory=$1 times=$2 file name base bases=() old=()
  mkdir -p "$times" || return
  for file in "$directory"/*.times; do
    [ -e "$file" ] || continue
    name=${file##*/}
    name=${name%.times}
    base=${name%.[0-9]*}
    case " ${bases[*]-} " in
      *" $base "*) ;;
      *)
        bases+=("$base")
        [ ! -f "$times/$base.times" ] || old+=("$times/$base.times")
        ;;
    esac
  done
  [ ${#bases[@]} -gt 0 ] || return 0
  # The times kept first, then this run's, which replace them.  The
  # directory comes through the environment: awk -v reads the
  # backslashes of a Windows path, such as TEMP's, as escapes.
  TIMES="$times" awk '
    BEGIN { directory = ENVIRON["TIMES"] }
    NF == 2 {
      base = FILENAME
      sub(/^.*\//, "", base)
      sub(/\.times$/, "", base)
      sub(/\.[0-9]+$/, "", base)
      time[base " " $1] = $2
    }
    END {
      for (key in time) {
        split(key, part, " ")
        print part[2], time[key] > (directory "/" part[1] ".times.new")
      }
    }' ${old[@]+"${old[@]}"} "$directory"/*.times
  for base in "${bases[@]}"; do
    mv "$times/$base.times.new" "$times/$base.times"
  done
}

report() {
  [ $# -eq 3 ] || [ $# -eq 4 ] || usage
  local directory=$1 jobs=$2 seconds=$3 times=${4-}
  local file name status count bad skip emacs=0 tests=0 unexpected=0 skipped=0
  local failed=() files=()
  for file in "$directory"/*.log "$directory"/*.log.status; do
    [ ! -e "$file" ] || files+=("$file")
  done
  [ ${#files[@]} -gt 0 ] || files=(/dev/null)
  # A line an Emacs: its name, status, and ERT's last count of tests,
  # unexpected results and skipped ones, "Ran N tests, N results as
  # expected, N unexpected[, N skipped] (...)".
  while read -r name status count bad skip; do
    emacs=$((emacs + 1))
    tests=$((tests + count))
    unexpected=$((unexpected + bad))
    skipped=$((skipped + skip))
    if [ "$status" != 0 ]; then
      failed+=("$name")
      echo
      echo "==== $name: status $status ===="
      grep -v '^ *passed ' "$directory/$name.log"
    fi
  done < <(
    # shellcheck disable=SC2016
    awk '
      FNR == 1 {
        file = FILENAME
        sub(/^.*\//, "", file)
        name = file
        sub(/\.log(\.status)?$/, "", name)
        seen[name] = 1
      }
      file ~ /\.status$/ { status[name] = $1; next }
      /^Ran [0-9]+ tests?, [0-9]+ results? as expected, [0-9]+ unexpected/ {
        count[name] = $2
        bad[name] = $8
        skip[name] = ($11 ~ /^skipped/) ? $10 : 0
      }
      END {
        for (name in seen)
          print name, (name in status) ? status[name] : "unknown", count[name] + 0, bad[name] + 0, skip[name] + 0
      }' "${files[@]}" | sort)
  [ -z "$times" ] || keep_times "$directory" "$times"
  echo
  echo "Ran $tests tests in $emacs batch Emacs, $jobs at once, in $seconds s:" \
    "$unexpected unexpected, $skipped skipped."
  if [ ${#failed[@]} -gt 0 ]; then
    echo "Failed: ${failed[*]}"
    echo "Logs: $directory"
    return 1
  fi
  rm -rf "$directory"
}

[ $# -ge 1 ] || usage
command=$1
shift
case $command in
  run) run "$@" ;;
  report) report "$@" ;;
  *) usage ;;
esac
