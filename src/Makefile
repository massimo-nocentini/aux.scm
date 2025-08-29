

test:
	cd test && csi -s test.scm
	cd test && csi -s test-stream.scm
	cd test && csi -s test-letcc.scm
	cd test && csi -s test-letnondeterministic.scm
	cd test && csi -s test-delimcc.scm
	cd test && csi -s test-timsort.scm

install:
	chicken-install -sudo

format:        
		scheme-indent -T 2 < test/test-letcc.scm > test/test-letcc.scm.tmp && mv test/test-letcc.scm.tmp test/test-letcc.scm
		scheme-indent -T 2 < test/test-letnondeterministic.scm > test/test-letnondeterministic.scm.tmp && mv test/test-letnondeterministic.scm.tmp test/test-letnondeterministic.scm
		scheme-indent -T 2 < test/test-delimcc.scm > test/test-delimcc.scm.tmp && mv test/test-delimcc.scm.tmp test/test-delimcc.scm
		scheme-indent -T 2 < test/test-timsort.scm > test/test-timsort.scm.tmp && mv test/test-timsort.scm.tmp test/test-timsort.scm
		scheme-indent -T 2 < test/test.scm > test/test.scm.tmp && mv test/test.scm.tmp test/test.scm
		scheme-indent -T 2 < test/test-stream.scm > test/test-stream.scm.tmp && mv test/test-stream.scm.tmp test/test-stream.scm
		
		scheme-indent -T 2 < aux.base.scm > aux.base.scm.tmp && mv aux.base.scm.tmp aux.base.scm
		scheme-indent -T 2 < aux.stream.scm > aux.stream.scm.tmp && mv aux.stream.scm.tmp aux.stream.scm
		scheme-indent -T 2 < aux.timsort.scm > aux.timsort.scm.tmp && mv aux.timsort.scm.tmp aux.timsort.scm
		scheme-indent -T 2 < aux.continuation.scm > aux.continuation.scm.tmp && mv aux.continuation.scm.tmp aux.continuation.scm
		scheme-indent -T 2 < aux.nondeterministic.scm > aux.nondeterministic.scm.tmp && mv aux.nondeterministic.scm.tmp aux.nondeterministic.scm