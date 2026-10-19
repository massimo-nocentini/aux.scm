#!/bin/sh
# Runs the λ suites, micro's own suites (from ../../../src/test) against the edited micro, the same suites with
# (aux kanren lambda) also imported (./micro-tests-with-lambda), and regenerates examples.out.
D=$(cd "$(dirname "$0")" && pwd)
CHICKEN_LIB=${CHICKEN_LIB:-$(chicken-install -repository)}
export CHICKEN_REPOSITORY_PATH=$D/repo:$CHICKEN_LIB
T=microkanren\ microkanren-aggregation\ microkanren-untagged\ microkanren-show\ microkanren-arith\ microkanren-date
cd "$D/test"
for t in microkanren-lambda microkanren-lambda-limits; do echo "test/$t $(timeout 300 csi -s $t.scm 2>&1 | tail -1)"; done
mkdir -p "$D/build/micro-tests" && cp "$D"/../../../src/test/microkanren*.scm "$D"/../../../src/test/microkanren-show.md "$D/build/micro-tests/" 2>/dev/null
for d in "$D/build/micro-tests" "$D/micro-tests-with-lambda"; do
  cd "$d"
  for t in $T; do echo "$(basename $d)/$t $(timeout 300 csi -s $t.scm 2>&1 | tail -1)"; done
done
cd "$D"; timeout 300 csi -s examples.scm > examples.out 2>&1 && echo "examples.scm ok"
