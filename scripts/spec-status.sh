#!/bin/sh
# Runs the spec examples and prints the open deviations (examples tagged bug=...).
cd "$(dirname "$0")/.."
mkdir -p target
sbt -batch -Dsbt.supershell=false "compiler/testOnly *SpecConformanceTest" > target/spec-status.log 2>&1
pass=$(grep -cE '^\[info\] - should [a-z0-9-]+ \(SPEC\.md:[0-9]+\)$' target/spec-status.log)
open=$(grep -c 'CANCELED' target/spec-status.log)
fail=$(grep -c '\*\*\* FAILED' target/spec-status.log)
echo "spec examples: $pass pass, $open open, $fail unexpected failures"
grep -E 'CANCELED|\*\*\* FAILED' target/spec-status.log \
  | sed -E 's/^\[info\] - should ([a-z0-9-]+) \(SPEC\.md:([0-9]+)\)( \[known bug: ([a-z0-9-]+)\])? (.*)$/\2|\1|\4|\5/' \
  | awk -F'|' '{ status = ($4 ~ /CANCELED/) ? "open" : "FAILING"; printf "  %-8s %-26s %-28s SPEC.md:%s\n", status, $3, $2, $1 }'
