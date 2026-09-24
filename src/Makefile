
CSI=csi
CSC=csc

.PHONY: echo clean test test-microkanren test-microkanren-show test-microkanren-arith \
        test-microkanren-date test-anthropic test-anthropic-live test-cml test-on-scheme test-learning-matchable install format simdjson-fetch \
        timsort-fetch foreigns-fetch json-bench bitcoin-colors

echo:
	echo "Catch all for the Makefile"

clean:
	rm -rf *.o *.so *.a *.link *.import.scm aux.install.sh aux.build.sh

test:
	cd test && ${CSI} -s test.scm
	cd test && ${CSI} -s category-list.scm
	cd test && ${CSI} -s stream.scm
	cd test && ${CSI} -s letcc.scm
	cd test && ${CSI} -s letnondeterministic.scm
	cd test && ${CSI} -s delimcc.scm
	cd test && ${CSI} -s timsort.scm
	cd test && ${CSI} -s timsort-trace-buffer.scm
	cd test && ${CSI} -s bootstrap.scm
	cd test && ${CSC} learning.scm bar.c -I/usr/local/include && ./learning && rm learning
	cd test && ${CSI} -s hansei.scm
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

test-microkanren:
	cd test && ${CSI} -s microkanren.scm

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
	scheme-indent -T 2 < aux.kanren.micro.scm > aux.kanren.micro.scm.tmp && mv aux.kanren.micro.scm.tmp aux.kanren.micro.scm
	scheme-indent -T 2 < aux.kanren.micro.show.scm > aux.kanren.micro.show.scm.tmp && mv aux.kanren.micro.show.scm.tmp aux.kanren.micro.show.scm
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