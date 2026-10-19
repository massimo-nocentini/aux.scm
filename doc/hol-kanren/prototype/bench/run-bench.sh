#!/bin/sh
# before: the prebuilt, unedited micro; after: the edited micro, no extension; ext: the edited micro with (aux kanren lambda) loaded.
D=$(cd "$(dirname "$0")/.." && pwd)
PRE=$D/../repo
cd "$D/bench"
for round in 1 2; do
  echo "--- round $round"
  echo "before"; CHICKEN_REPOSITORY_PATH=$PRE:/home/mnocentini/.local/lib/chicken/12 ./fo-before
  echo "after";  CHICKEN_REPOSITORY_PATH=$D/repo:/home/mnocentini/.local/lib/chicken/12 ./fo-after
  echo "ext";    CHICKEN_REPOSITORY_PATH=$D/repo:/home/mnocentini/.local/lib/chicken/12 ./fo-ext
done
echo "--- λ workloads"; CHICKEN_REPOSITORY_PATH=$D/repo:/home/mnocentini/.local/lib/chicken/12 ./bench-lambda
