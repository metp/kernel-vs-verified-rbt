.PHONY: cov
cov: harness_wrapper harness_wrapper_core klee_wrapper klee_wrapper_core

harness_wrapper: main_harness.c rbtree.c
	gcc -I linux -fprofile-arcs -ftest-coverage $^ -o harness_wrapper

harness_wrapper_core: main_harness.c rbtree.c
	gcc -I linux -fprofile-arcs -ftest-coverage -DCORE $^ -o harness_wrapper_core

klee_wrapper: main_klee.c rbtree.c
	gcc -I linux -fprofile-arcs -ftest-coverage -DCOVERAGE $^ -o klee_wrapper

klee_wrapper_core: main_klee.c rbtree.c
	gcc -I linux -fprofile-arcs -ftest-coverage -DCOVERAGE -DCORE $^ -o klee_wrapper_core

.PHONY: symbolic
symbolic: klee_wrapper.bc
	klee -only-output-states-covering-new --libc=uclibc --posix-runtime $^ -sym-stdin $(STDIN_SIZE)
	ktest-tool --extract stdin klee-last/*.ktest

klee_wrapper.bc: main_klee.bc rbtree.bc
	llvm-link $? -o klee_wrapper.bc

main_klee.bc rbtree.bc: main_klee.c rbtree.c
	clang -I linux -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone $^

.PHONY: clean
clean:
	@$(RM) *.bc *.gcno *.gcov *.gcda klee_wrapper klee_wrapper_core harness_wrapper harness_wrapper_core
