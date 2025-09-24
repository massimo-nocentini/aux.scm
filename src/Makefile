

test:
	cd test && csi -s test.scm
	cd test && csi -s stream.scm
	cd test && csi -s letcc.scm
	cd test && csi -s letnondeterministic.scm
	cd test && csi -s delimcc.scm
	cd test && csi -s timsort.scm
	cd test && csi -s bootstrap.scm
	cd test && csc learning.scm bar.c && ./learning && rm learning
	cd test && csi -s hansei.scm
	cd test && csi -s microkanren.scm
	cd test && csi -s fds.queue.scm
	cd test && csi -s fds.sbral.scm

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

	#sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba' aux.base.scm > aux.base.scm.tmp && mv aux.base.scm.tmp aux.base.scm
	#sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba' aux.stream.scm > aux.stream.scm.tmp && mv aux.stream.scm.tmp aux.stream.scm
	#sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba' aux.timsort.scm > aux.timsort.scm.tmp && mv aux.timsort.scm.tmp aux.timsort.scm
	#sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba' aux.continuation.scm > aux.continuation.scm.tmp && mv aux.continuation.scm.tmp aux.continuation.scm
	#sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba' aux.unittest.scm > aux.unittest.scm.tmp && mv aux.unittest.scm.tmp aux.unittest.scm 
	#sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba' aux.fds.queue.scm > aux.fds.queue.scm.tmp && mv aux.fds.queue.scm.tmp aux.fds.queue.scm
	#sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba' aux.fds.sbral.scm > aux.fds.sbral.scm.tmp && mv aux.fds.sbral.scm.tmp aux.fds.sbral.scm 
	#sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba' aux.nondeterministic.scm > aux.nondeterministic.scm.tmp && mv aux.nondeterministic.scm.tmp aux.nondeterministic.scm
	#sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba' aux.sxml.scm > aux.sxml.scm.tmp && mv aux.sxml.scm.tmp aux.sxml.scm
	#sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba' aux.hansei.scm > aux.hansei.scm.tmp && mv aux.hansei.scm.tmp aux.hansei.scm
	#sed -e :a -e '/^\n\n*$/{$d;N;};/\n$/ba' aux.kanren.micro.scm > aux.kanren.micro.scm.tmp && mv aux.kanren.micro.scm.tmp aux.kanren.micro.scm	

simdjson-fetch:
	rm -rf simdjson.cpp simdjson.h test/twitter.json
	wget --no-verbose https://github.com/simdjson/simdjson/releases/latest/download/simdjson.cpp
	wget --no-verbose https://github.com/simdjson/simdjson/releases/latest/download/simdjson.h
	cd test && wget --no-verbose https://raw.githubusercontent.com/simdjson/simdjson/refs/heads/master/jsonexamples/twitter.json
	
simdjson-compile:
	clang++ -O3 -Wall -g -shared -fPIC chicken-simdjson.cpp simdjson.cpp -o libchicken-simdjson.so
	
simdjson-install:
	mv libchicken-simdjson.so /usr/local/lib
	
