#!/bin/sh
# Builds a private CHICKEN repository ./repo holding the modules (aux kanren micro) needs, the edited micro and show
# modules of this directory, and (aux kanren lambda). Unchanged modules come from ../../../src. Nothing outside
# this directory is written. Run ./run-tests.sh afterwards.
set -e
D=$(cd "$(dirname "$0")" && pwd)
SRC=$D/../../../src
CHICKEN_LIB=${CHICKEN_LIB:-$(chicken-install -repository)}
INCLUDE=${INCLUDE:-$HOME/.local/include}
export CHICKEN_REPOSITORY_PATH=$D/repo:$CHICKEN_LIB
mkdir -p "$D/repo" "$D/build"
cd "$D/build"
for m in aux.base aux.sxml aux.unittest aux.category.monad aux.continuation aux.continuation.delimited aux.stream \
         aux.fds.sbral aux.kanren.micro aux.kanren.micro.show aux.kanren.arith aux.kanren.date aux.kanren.lambda; do
  f=$D/$m.scm; [ -e "$f" ] || f=$SRC/$m.scm
  cp "$f" $m.scm
  csc -O2 -d1 -s -J -I"$INCLUDE" $m.scm -o $m.so
  cp $m.so "$D/repo/"
  for i in *.import.scm; do csc -s -O2 $i -o ${i%.scm}.so && cp ${i%.scm}.so $i "$D/repo/" && rm $i; done
done
echo "export CHICKEN_REPOSITORY_PATH=$CHICKEN_REPOSITORY_PATH"
