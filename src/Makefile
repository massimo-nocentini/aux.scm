	
test: test-microkanren
	cd test && csi -s test.scm
	cd test && csi -s stream.scm
	cd test && csi -s letcc.scm
	cd test && csi -s letnondeterministic.scm
	cd test && csi -s delimcc.scm
	cd test && csi -s timsort.scm
	cd test && csi -s bootstrap.scm
	cd test && csc learning.scm bar.c -I/usr/local/include && ./learning && rm learning
	cd test && csi -s hansei.scm
	cd test && csi -s fds.queue.scm
	cd test && csi -s fds.sbral.scm
	cd test && csi -s judy.scm

test-microkanren:
	cd test && csi -s microkanren.scm

install:
	chicken-install -sudo

format:        
	scheme-indent -T 2 < test/letcc.scm > test/letcc.scm.tmp && mv test/letcc.scm.tmp test/letcc.scm
	scheme-indent -T 2 < test/letnondeterministic.scm > test/letnondeterministic.scm.tmp && mv test/letnondeterministic.scm.tmp test/letnondeterministic.scm
	scheme-indent -T 2 < test/delimcc.scm > test/delimcc.scm.tmp && mv test/delimcc.scm.tmp test/delimcc.scm
	scheme-indent -T 2 < test/timsort.scm > test/timsort.scm.tmp && mv test/timsort.scm.tmp test/timsort.scm
	scheme-indent -T 2 < test/test.scm > test/test.scm.tmp && mv test/test.scm.tmp test/test.scm
	scheme-indent -T 2 < test/stream.scm > test/stream.scm.tmp && mv test/stream.scm.tmp test/stream.scm
	scheme-indent -T 2 < test/bootstrap.scm > test/bootstrap.scm.tmp && mv test/bootstrap.scm.tmp test/bootstrap.scm
	scheme-indent -T 2 < test/learning.scm > test/learning.scm.tmp && mv test/learning.scm.tmp test/learning.scm
	scheme-indent -T 2 < test/hansei.scm > test/hansei.scm.tmp && mv test/hansei.scm.tmp test/hansei.scm
	scheme-indent -T 2 < test/microkanren.scm > test/microkanren.scm.tmp && mv test/microkanren.scm.tmp test/microkanren.scm
	scheme-indent -T 2 < test/fds.queue.scm > test/fds.queue.scm.tmp && mv test/fds.queue.scm.tmp test/fds.queue.scm
	scheme-indent -T 2 < test/fds.sbral.scm > test/fds.sbral.scm.tmp && mv test/fds.sbral.scm.tmp test/fds.sbral.scm
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

simdjson-fetch:
	rm -rf simdjson.cpp simdjson.h #test/twitter.json
	wget --no-verbose https://github.com/simdjson/simdjson/releases/latest/download/simdjson.cpp
	wget --no-verbose https://github.com/simdjson/simdjson/releases/latest/download/simdjson.h
	#cd test && wget --no-verbose https://raw.githubusercontent.com/simdjson/simdjson/refs/heads/master/jsonexamples/twitter.json
	
timsort-fetch:
	rm -rf timsort.c timsort.h
	wget --no-verbose https://raw.githubusercontent.com/massimo-nocentini/timsort.c/refs/heads/master/src/timsort.c
	wget --no-verbose https://raw.githubusercontent.com/massimo-nocentini/timsort.c/refs/heads/master/src/timsort.h

foreigns-fetch: timsort-fetch simdjson-fetch

json-bench:
	cd test && rm -rf users_100k.json users_1.7m.json && \
		wget --no-verbose blob:https://jsoneditoronline.org/1e851c21-64a5-4364-b99a-ab48d0a9e710 -O users_1.7m.json