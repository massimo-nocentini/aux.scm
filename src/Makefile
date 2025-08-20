

test:
	cd test && \
		csi -s test.scm && \
		csi -s test-letcc.scm && \
		csi -s test-letnondeterministic.scm && \
		csi -s test-delimcc.scm && \
		csi -s test-timsort.scm

install:
	chicken-install -sudo

format:        
		scheme-indent -T 2 < test/test-letcc.scm > test/test-letcc.scm.tmp && mv test/test-letcc.scm.tmp test/test-letcc.scm
		scheme-indent -T 2 < test/test-letnondeterministic.scm > test/test-letnondeterministic.scm.tmp && mv test/test-letnondeterministic.scm.tmp test/test-letnondeterministic.scm
		scheme-indent -T 2 < test/test-delimcc.scm > test/test-delimcc.scm.tmp && mv test/test-delimcc.scm.tmp test/test-delimcc.scm
		scheme-indent -T 2 < test/test-timsort.scm > test/test-timsort.scm.tmp && mv test/test-timsort.scm.tmp test/test-timsort.scm
		scheme-indent -T 2 < test/test.scm > test/test.scm.tmp && mv test/test.scm.tmp test/test.scm
		scheme-indent -T 2 < aux.scm > aux.scm.tmp && mv aux.scm.tmp aux.scm