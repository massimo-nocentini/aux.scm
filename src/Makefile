
CSI=csi
CSC=csc

.PHONY: echo clean test test-microkanren test-microkanren-show test-microkanren-arith \
        test-microkanren-date test-microkanren-lambda test-microkanren-with-lambda test-anthropic test-anthropic-live test-cml test-on-scheme test-lua test-learning-matchable test-hakaru10-long test-hansei-long bench-hakaru10 install format simdjson-fetch \
        timsort-fetch foreigns-fetch json-bench bitcoin-colors test-pie

echo:
	echo "Catch all for the Makefile"

clean:
	rm -rf *.o *.so *.a *.link *.import.scm aux.install.sh aux.build.sh

test:
	cd test && ${CSI} -s test.scm
	cd test && ${CSI} -s category-list.scm
	cd test && ${CSI} -s category-prob.scm
	cd test && ${CSI} -s category-maybe.scm
	cd test && ${CSI} -s category-ziplist.scm
	cd test && ${CSI} -s category-difflist.scm
	cd test && ${CSI} -s category-state.scm
	cd test && ${CSI} -s stream.scm
	cd test && ${CSI} -s letcc.scm
	cd test && ${CSI} -s letnondeterministic.scm
	cd test && ${CSI} -s delimcc.scm
	cd test && ${CSI} -s timsort.scm
	cd test && ${CSI} -s timsort-trace-buffer.scm
	cd test && ${CSI} -s bootstrap.scm
	cd test && ${CSC} learning.scm bar.c -I/usr/local/include && ./learning && rm learning
	cd test && ${CSI} -s hansei.scm
	cd test && ${CSI} -s hansei-random.scm
	cd test && ${CSI} -s hansei-lazy.scm
	cd test && ${CSI} -s hansei-examples.scm
	cd test && ${CSI} -s hansei-logic.scm
	cd test && ${CSI} -s hakaru10-random.scm
	cd test && ${CSI} -s hakaru10-distribution.scm
	cd test && ${CSI} -s hakaru10-util.scm
	cd test && ${CSI} -s hakaru10.scm
	cd test && ${CSI} -s hakaru10-exact.scm
	cd test && ${CSI} -s hakaru10-examples.scm
	cd test && ${CSI} -s fds.queue.scm
	cd test && ${CSI} -s fds.sbral.scm
# 	cd test && ${CSI} -s judy.scm
	cd test && ${CSI} -s dmatch.scm
	cd test && ${CSI} -s microkanren.scm
	cd test && ${CSI} -s microkanren-aggregation.scm
	cd test && ${CSI} -s microkanren-untagged.scm
	cd test && ${CSI} -s microkanren-show.scm
	cd test && ${CSI} -s microkanren-arith.scm
	cd test && ${CSI} -s microkanren-date.scm
	cd test && ${CSI} -s microkanren-extension.scm
	cd test && ${CSI} -s microkanren-lambda.scm
	cd test && ${CSI} -s microkanren-lambda-limits.scm
	cd test && for t in microkanren microkanren-aggregation microkanren-untagged microkanren-show microkanren-arith microkanren-date; do ${CSI} -R aux.kanren.lambda -s $$t.scm || exit 1; done
	cd test && ${CSI} -s anthropic.scm
	cd test && ${CSI} -s cml.scm
	cd test && ${CSI} -s cml-lib.scm
	cd test && ${CSI} -s cml-io.scm
	cd test && ${CSI} -s commons.scm
	cd test && ${CSI} -s tabling.scm
	cd test && ${CSI} -s variable.scm
	cd test && ${CSI} -s promise.scm
	cd test && ${CSI} -s learning-chicken.scm
	cd test && ${CSI} -s ackermann.scm
	cd test && ${CSI} -s continuation.classic.scm
	cd test && ${CSI} -s continuation.amb.scm
	cd test && ${CSI} -s continuation.escaper.scm
	cd test && ${CSI} -s continuation.collatz.scm
	cd test && ${CSI} -s stream.sicp.scm
	cd test && ${CSI} -s fds.unionfind.scm
	cd test && ${CSI} -s schemer.seasoned.scm
	cd test && ${CSI} -s machine.env.scm
	cd test && ${CSI} -s machine.sc.scm
	cd test && ${CSI} -s machine.secd.scm
	cd test && ${CSI} -s microkanren-unionfind.scm
	cd test && ${CSI} -s games.dice-of-doom.scm
	cd test && ${CSI} -s sxml.scm
	cd test && ${CSI} -s pie-units.scm
	cd test && ${CSI} -s pie.scm
	cd test && ${CSI} -s pie-programs.scm

# The tests of (aux pie), the module+ tests of upstream, tests.rkt, and whole programs against Racket Pie's output.
test-pie:
	cd test && ${CSI} -s pie-units.scm
	cd test && ${CSI} -s pie.scm
	cd test && ${CSI} -s pie-programs.scm

test-microkanren:
	cd test && ${CSI} -s microkanren.scm
	cd test && ${CSI} -s microkanren-extension.scm

test-microkanren-lambda:
	cd test && ${CSI} -s microkanren-lambda.scm
	cd test && ${CSI} -s microkanren-lambda-limits.scm

# micro's own suites again, with (aux kanren lambda) loaded, so that its extension is registered.
test-microkanren-with-lambda:
	cd test && for t in microkanren microkanren-aggregation microkanren-untagged microkanren-show microkanren-arith microkanren-date; do ${CSI} -R aux.kanren.lambda -s $$t.scm || exit 1; done

test-microkanren-show:
	cd test && ${CSI} -s microkanren-show.scm

test-microkanren-arith:
	cd test && ${CSI} -s microkanren-arith.scm

test-microkanren-date:
	cd test && ${CSI} -s microkanren-date.scm

test-anthropic:
	cd test && ${CSI} -s anthropic.scm

test-anthropic-live:
	cd test && ${CSI} -s anthropic-live.scm

test-cml:
	cd test && ${CSI} -s cml.scm
	cd test && ${CSI} -s cml-lib.scm
	cd test && ${CSI} -s cml-io.scm

test-on-scheme:
	cd test && ${CSI} -s commons.scm
	cd test && ${CSI} -s tabling.scm
	cd test && ${CSI} -s variable.scm
	cd test && ${CSI} -s promise.scm
	cd test && ${CSI} -s learning-chicken.scm
	cd test && ${CSI} -s ackermann.scm
	cd test && ${CSI} -s continuation.classic.scm
	cd test && ${CSI} -s continuation.amb.scm
	cd test && ${CSI} -s continuation.escaper.scm
	cd test && ${CSI} -s continuation.collatz.scm
	cd test && ${CSI} -s stream.sicp.scm
	cd test && ${CSI} -s fds.unionfind.scm
	cd test && ${CSI} -s schemer.seasoned.scm
	cd test && ${CSI} -s machine.env.scm
	cd test && ${CSI} -s machine.sc.scm
	cd test && ${CSI} -s machine.secd.scm
	cd test && ${CSI} -s microkanren-unionfind.scm
	cd test && ${CSI} -s games.dice-of-doom.scm

test-learning-matchable:
	cd test && ${CSI} -s learning-matchable.scm

# (aux hansei): zebra, the parser replays of 30000 samples, and the statistical sampling tests.
test-hansei-long:
	cd test && ${CSI} -s hansei-long.scm

# (aux hakaru10): the statistical suite over several seeds, and the bird migration model on the
# real data when HAKARU10_BIRDS_DIR names a folder with its CSV files: not part of `test`.
test-hakaru10-long:
	cd test && HAKARU10_SEEDS=17,1,2,3,4 ${CSI} -s hakaru10.scm
	cd test && ${CSI} -s hakaru10-birds.scm

bench-hakaru10:
	cd test && ${CSC} -O3 hakaru10-bench.scm && ./hakaru10-bench 10 10000

# (aux lua) needs Lua 5.5 (headers and liblua): not part of `test`.
test-lua:
	cd test && ${CSI} -s lua.scm

install:
	chicken-install -sudo

format:        
	scheme-indent -T 2 < test/letcc.scm > test/letcc.scm.tmp && mv test/letcc.scm.tmp test/letcc.scm
	scheme-indent -T 2 < test/letnondeterministic.scm > test/letnondeterministic.scm.tmp && mv test/letnondeterministic.scm.tmp test/letnondeterministic.scm
	scheme-indent -T 2 < test/delimcc.scm > test/delimcc.scm.tmp && mv test/delimcc.scm.tmp test/delimcc.scm
	scheme-indent -T 2 < test/timsort.scm > test/timsort.scm.tmp && mv test/timsort.scm.tmp test/timsort.scm
	scheme-indent -T 2 < test/timsort-trace-buffer.scm > test/timsort-trace-buffer.scm.tmp && mv test/timsort-trace-buffer.scm.tmp test/timsort-trace-buffer.scm
	scheme-indent -T 2 < test/test.scm > test/test.scm.tmp && mv test/test.scm.tmp test/test.scm
	scheme-indent -T 2 < test/stream.scm > test/stream.scm.tmp && mv test/stream.scm.tmp test/stream.scm
	scheme-indent -T 2 < test/bootstrap.scm > test/bootstrap.scm.tmp && mv test/bootstrap.scm.tmp test/bootstrap.scm
	scheme-indent -T 2 < test/learning.scm > test/learning.scm.tmp && mv test/learning.scm.tmp test/learning.scm
	scheme-indent -T 2 < test/hansei.scm > test/hansei.scm.tmp && mv test/hansei.scm.tmp test/hansei.scm
	scheme-indent -T 2 < test/hansei-random.scm > test/hansei-random.scm.tmp && mv test/hansei-random.scm.tmp test/hansei-random.scm
	scheme-indent -T 2 < test/hansei-lazy.scm > test/hansei-lazy.scm.tmp && mv test/hansei-lazy.scm.tmp test/hansei-lazy.scm
	scheme-indent -T 2 < test/hansei-examples.scm > test/hansei-examples.scm.tmp && mv test/hansei-examples.scm.tmp test/hansei-examples.scm
	scheme-indent -T 2 < test/hansei-logic.scm > test/hansei-logic.scm.tmp && mv test/hansei-logic.scm.tmp test/hansei-logic.scm
	scheme-indent -T 2 < test/hansei-long.scm > test/hansei-long.scm.tmp && mv test/hansei-long.scm.tmp test/hansei-long.scm
	scheme-indent -T 2 < test/hakaru10-random.scm > test/hakaru10-random.scm.tmp && mv test/hakaru10-random.scm.tmp test/hakaru10-random.scm
	scheme-indent -T 2 < test/hakaru10-distribution.scm > test/hakaru10-distribution.scm.tmp && mv test/hakaru10-distribution.scm.tmp test/hakaru10-distribution.scm
	scheme-indent -T 2 < test/hakaru10-util.scm > test/hakaru10-util.scm.tmp && mv test/hakaru10-util.scm.tmp test/hakaru10-util.scm
	scheme-indent -T 2 < test/hakaru10.scm > test/hakaru10.scm.tmp && mv test/hakaru10.scm.tmp test/hakaru10.scm
	scheme-indent -T 2 < test/hakaru10-exact.scm > test/hakaru10-exact.scm.tmp && mv test/hakaru10-exact.scm.tmp test/hakaru10-exact.scm
	scheme-indent -T 2 < test/hakaru10-examples.scm > test/hakaru10-examples.scm.tmp && mv test/hakaru10-examples.scm.tmp test/hakaru10-examples.scm
	scheme-indent -T 2 < test/hakaru10-bench.scm > test/hakaru10-bench.scm.tmp && mv test/hakaru10-bench.scm.tmp test/hakaru10-bench.scm
	scheme-indent -T 2 < test/hakaru10-birds.scm > test/hakaru10-birds.scm.tmp && mv test/hakaru10-birds.scm.tmp test/hakaru10-birds.scm
	scheme-indent -T 2 < test/microkanren.scm > test/microkanren.scm.tmp && mv test/microkanren.scm.tmp test/microkanren.scm
	scheme-indent -T 2 < test/fds.queue.scm > test/fds.queue.scm.tmp && mv test/fds.queue.scm.tmp test/fds.queue.scm
	scheme-indent -T 2 < test/fds.sbral.scm > test/fds.sbral.scm.tmp && mv test/fds.sbral.scm.tmp test/fds.sbral.scm
	scheme-indent -T 2 < test/anthropic.scm > test/anthropic.scm.tmp && mv test/anthropic.scm.tmp test/anthropic.scm
	scheme-indent -T 2 < test/anthropic-live.scm > test/anthropic-live.scm.tmp && mv test/anthropic-live.scm.tmp test/anthropic-live.scm
	scheme-indent -T 2 < test/cml.scm > test/cml.scm.tmp && mv test/cml.scm.tmp test/cml.scm
	scheme-indent -T 2 < test/cml-lib.scm > test/cml-lib.scm.tmp && mv test/cml-lib.scm.tmp test/cml-lib.scm
	scheme-indent -T 2 < test/cml-io.scm > test/cml-io.scm.tmp && mv test/cml-io.scm.tmp test/cml-io.scm
	scheme-indent -T 2 < aux.base.scm > aux.base.scm.tmp && mv aux.base.scm.tmp aux.base.scm 
	scheme-indent -T 2 < aux.libc.scm > aux.libc.scm.tmp && mv aux.libc.scm.tmp aux.libc.scm
	scheme-indent -T 2 < aux.simdjson.scm > aux.simdjson.scm.tmp && mv aux.simdjson.scm.tmp aux.simdjson.scm
	scheme-indent -T 2 < aux.stream.scm > aux.stream.scm.tmp && mv aux.stream.scm.tmp aux.stream.scm
	scheme-indent -T 2 < aux.timsort.scm > aux.timsort.scm.tmp && mv aux.timsort.scm.tmp aux.timsort.scm
	scheme-indent -T 2 < aux.continuation.scm > aux.continuation.scm.tmp && mv aux.continuation.scm.tmp aux.continuation.scm
	scheme-indent -T 2 < aux.unittest.scm > aux.unittest.scm.tmp && mv aux.unittest.scm.tmp aux.unittest.scm 
	scheme-indent -T 2 < aux.fds.queue.scm > aux.fds.queue.scm.tmp && mv aux.fds.queue.scm.tmp aux.fds.queue.scm
	scheme-indent -T 2 < aux.fds.sbral.scm > aux.fds.sbral.scm.tmp && mv aux.fds.sbral.scm.tmp aux.fds.sbral.scm 
	scheme-indent -T 2 < aux.nondeterministic.scm > aux.nondeterministic.scm.tmp && mv aux.nondeterministic.scm.tmp aux.nondeterministic.scm
	scheme-indent -T 2 < aux.sxml.scm > aux.sxml.scm.tmp && mv aux.sxml.scm.tmp aux.sxml.scm
	scheme-indent -T 2 < aux.hansei.scm > aux.hansei.scm.tmp && mv aux.hansei.scm.tmp aux.hansei.scm
	scheme-indent -T 2 < aux.hansei.pmap.scm > aux.hansei.pmap.scm.tmp && mv aux.hansei.pmap.scm.tmp aux.hansei.pmap.scm
	scheme-indent -T 2 < aux.hansei.random.scm > aux.hansei.random.scm.tmp && mv aux.hansei.random.scm.tmp aux.hansei.random.scm
	scheme-indent -T 2 < aux.hansei.lazy.scm > aux.hansei.lazy.scm.tmp && mv aux.hansei.lazy.scm.tmp aux.hansei.lazy.scm
	scheme-indent -T 2 < aux.hansei.parsec.scm > aux.hansei.parsec.scm.tmp && mv aux.hansei.parsec.scm.tmp aux.hansei.parsec.scm
	scheme-indent -T 2 < aux.hakaru10.random.scm > aux.hakaru10.random.scm.tmp && mv aux.hakaru10.random.scm.tmp aux.hakaru10.random.scm
	scheme-indent -T 2 < aux.hakaru10.distribution.scm > aux.hakaru10.distribution.scm.tmp && mv aux.hakaru10.distribution.scm.tmp aux.hakaru10.distribution.scm
	scheme-indent -T 2 < aux.hakaru10.util.scm > aux.hakaru10.util.scm.tmp && mv aux.hakaru10.util.scm.tmp aux.hakaru10.util.scm
	scheme-indent -T 2 < aux.hakaru10.engine.scm > aux.hakaru10.engine.scm.tmp && mv aux.hakaru10.engine.scm.tmp aux.hakaru10.engine.scm
	scheme-indent -T 2 < aux.hakaru10.exact.scm > aux.hakaru10.exact.scm.tmp && mv aux.hakaru10.exact.scm.tmp aux.hakaru10.exact.scm
	scheme-indent -T 2 < aux.hakaru10.scm > aux.hakaru10.scm.tmp && mv aux.hakaru10.scm.tmp aux.hakaru10.scm
	scheme-indent -T 2 < aux.kanren.micro.scm > aux.kanren.micro.scm.tmp && mv aux.kanren.micro.scm.tmp aux.kanren.micro.scm
	scheme-indent -T 2 < aux.kanren.micro.show.scm > aux.kanren.micro.show.scm.tmp && mv aux.kanren.micro.show.scm.tmp aux.kanren.micro.show.scm
	scheme-indent -T 2 < aux.kanren.lambda.scm > aux.kanren.lambda.scm.tmp && mv aux.kanren.lambda.scm.tmp aux.kanren.lambda.scm
	scheme-indent -T 2 < aux.anthropic.scm > aux.anthropic.scm.tmp && mv aux.anthropic.scm.tmp aux.anthropic.scm
	scheme-indent -T 2 < aux.cml.scm > aux.cml.scm.tmp && mv aux.cml.scm.tmp aux.cml.scm
	scheme-indent -T 2 < test/commons.scm > test/commons.scm.tmp && mv test/commons.scm.tmp test/commons.scm
	scheme-indent -T 2 < test/tabling.scm > test/tabling.scm.tmp && mv test/tabling.scm.tmp test/tabling.scm
	scheme-indent -T 2 < test/variable.scm > test/variable.scm.tmp && mv test/variable.scm.tmp test/variable.scm
	scheme-indent -T 2 < test/promise.scm > test/promise.scm.tmp && mv test/promise.scm.tmp test/promise.scm
	scheme-indent -T 2 < test/learning-chicken.scm > test/learning-chicken.scm.tmp && mv test/learning-chicken.scm.tmp test/learning-chicken.scm
	scheme-indent -T 2 < test/ackermann.scm > test/ackermann.scm.tmp && mv test/ackermann.scm.tmp test/ackermann.scm
	scheme-indent -T 2 < test/continuation.classic.scm > test/continuation.classic.scm.tmp && mv test/continuation.classic.scm.tmp test/continuation.classic.scm
	scheme-indent -T 2 < test/continuation.amb.scm > test/continuation.amb.scm.tmp && mv test/continuation.amb.scm.tmp test/continuation.amb.scm
	scheme-indent -T 2 < test/continuation.escaper.scm > test/continuation.escaper.scm.tmp && mv test/continuation.escaper.scm.tmp test/continuation.escaper.scm
	scheme-indent -T 2 < test/continuation.collatz.scm > test/continuation.collatz.scm.tmp && mv test/continuation.collatz.scm.tmp test/continuation.collatz.scm
	scheme-indent -T 2 < test/stream.sicp.scm > test/stream.sicp.scm.tmp && mv test/stream.sicp.scm.tmp test/stream.sicp.scm
	scheme-indent -T 2 < test/fds.unionfind.scm > test/fds.unionfind.scm.tmp && mv test/fds.unionfind.scm.tmp test/fds.unionfind.scm
	scheme-indent -T 2 < test/schemer.seasoned.scm > test/schemer.seasoned.scm.tmp && mv test/schemer.seasoned.scm.tmp test/schemer.seasoned.scm
	scheme-indent -T 2 < test/machine.env.scm > test/machine.env.scm.tmp && mv test/machine.env.scm.tmp test/machine.env.scm
	scheme-indent -T 2 < test/machine.sc.scm > test/machine.sc.scm.tmp && mv test/machine.sc.scm.tmp test/machine.sc.scm
	scheme-indent -T 2 < test/machine.secd.scm > test/machine.secd.scm.tmp && mv test/machine.secd.scm.tmp test/machine.secd.scm
	scheme-indent -T 2 < test/microkanren-unionfind.scm > test/microkanren-unionfind.scm.tmp && mv test/microkanren-unionfind.scm.tmp test/microkanren-unionfind.scm
	scheme-indent -T 2 < test/games.dice-of-doom.scm > test/games.dice-of-doom.scm.tmp && mv test/games.dice-of-doom.scm.tmp test/games.dice-of-doom.scm
	scheme-indent -T 2 < test/learning-matchable.scm > test/learning-matchable.scm.tmp && mv test/learning-matchable.scm.tmp test/learning-matchable.scm
	scheme-indent -T 2 < aux.commons.scm > aux.commons.scm.tmp && mv aux.commons.scm.tmp aux.commons.scm
	scheme-indent -T 2 < aux.tabling.scm > aux.tabling.scm.tmp && mv aux.tabling.scm.tmp aux.tabling.scm
	scheme-indent -T 2 < aux.variable.scm > aux.variable.scm.tmp && mv aux.variable.scm.tmp aux.variable.scm
	scheme-indent -T 2 < aux.continuation.classic.scm > aux.continuation.classic.scm.tmp && mv aux.continuation.classic.scm.tmp aux.continuation.classic.scm
	scheme-indent -T 2 < aux.stream.sicp.scm > aux.stream.sicp.scm.tmp && mv aux.stream.sicp.scm.tmp aux.stream.sicp.scm
	scheme-indent -T 2 < aux.stream.series.scm > aux.stream.series.scm.tmp && mv aux.stream.series.scm.tmp aux.stream.series.scm
	scheme-indent -T 2 < aux.fds.unionfind.scm > aux.fds.unionfind.scm.tmp && mv aux.fds.unionfind.scm.tmp aux.fds.unionfind.scm
	scheme-indent -T 2 < aux.schemer.seasoned.scm > aux.schemer.seasoned.scm.tmp && mv aux.schemer.seasoned.scm.tmp aux.schemer.seasoned.scm
	scheme-indent -T 2 < aux.machine.env.scm > aux.machine.env.scm.tmp && mv aux.machine.env.scm.tmp aux.machine.env.scm
	scheme-indent -T 2 < aux.machine.sc.scm > aux.machine.sc.scm.tmp && mv aux.machine.sc.scm.tmp aux.machine.sc.scm
	scheme-indent -T 2 < aux.machine.secd.scm > aux.machine.secd.scm.tmp && mv aux.machine.secd.scm.tmp aux.machine.secd.scm
	scheme-indent -T 2 < aux.kanren.unionfind.scm > aux.kanren.unionfind.scm.tmp && mv aux.kanren.unionfind.scm.tmp aux.kanren.unionfind.scm
	scheme-indent -T 2 < aux.games.dice-of-doom.scm > aux.games.dice-of-doom.scm.tmp && mv aux.games.dice-of-doom.scm.tmp aux.games.dice-of-doom.scm

simdjson-fetch:
	rm -rf simdjson.cpp simdjson.h #test/twitter.json
	wget --no-verbose https://github.com/simdjson/simdjson/releases/latest/download/simdjson.cpp
	wget --no-verbose https://github.com/simdjson/simdjson/releases/latest/download/simdjson.h
	
timsort-fetch:
	rm -rf timsort.c timsort.h
	wget --no-verbose https://raw.githubusercontent.com/massimo-nocentini/timsort.c/refs/heads/master/src/timsort.c
	wget --no-verbose https://raw.githubusercontent.com/massimo-nocentini/timsort.c/refs/heads/master/src/timsort.h

foreigns-fetch: timsort-fetch simdjson-fetch

json-bench:
	# first take json files from https://jsoneditoronline.org/indepth/datasets/json-file-example/#a-large-json-document
	${CSI} -s test/json-bench.scm

bitcoin-colors:
	cd test && docker run -it --rm -v ./:/home/ubuntu --entrypoint bash ghcr.io/massimo-nocentini/aux.scm:master