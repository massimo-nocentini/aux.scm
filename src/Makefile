

test:
	cd test && csi -s test.scm
	cd test && csi -s stream.scm
	cd test && csi -s letcc.scm
	cd test && csi -s letnondeterministic.scm
	cd test && csi -s delimcc.scm
	cd test && csi -s timsort.scm
	cd test && csi -s bootstrap.scm
	cd test && csc learning.scm && ./learning

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

		scheme-indent -T 2 < aux.base.scm > aux.base.scm.tmp && mv aux.base.scm.tmp aux.base.scm
		scheme-indent -T 2 < aux.stream.scm > aux.stream.scm.tmp && mv aux.stream.scm.tmp aux.stream.scm
		scheme-indent -T 2 < aux.timsort.scm > aux.timsort.scm.tmp && mv aux.timsort.scm.tmp aux.timsort.scm
		scheme-indent -T 2 < aux.continuation.scm > aux.continuation.scm.tmp && mv aux.continuation.scm.tmp aux.continuation.scm
		scheme-indent -T 2 < aux.unittest.scm > aux.unittest.scm.tmp && mv aux.unittest.scm.tmp aux.unittest.scm 
		scheme-indent -T 2 < aux.nondeterministic.scm > aux.nondeterministic.scm.tmp && mv aux.nondeterministic.scm.tmp aux.nondeterministic.scm
		scheme-indent -T 2 < aux.sxml.scm > aux.sxml.scm.tmp && mv aux.sxml.scm.tmp aux.sxml.scm