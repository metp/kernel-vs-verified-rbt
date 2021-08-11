harness_wrapper: main_harness.c rbtree.c
	gcc -I linux -fprofile-arcs -ftest-coverage $^ -o harness_wrapper

.PHONY:
symbolic: klee_wrapper.bc
	klee -only-output-states-covering-new --libc=uclibc --posix-runtime $^ -sym-stdin $(STDIN_SIZE)
	ktest-tool --extract stdin klee-last/*.ktest

klee_wrapper.bc: main_klee.bc rbtree.bc
	llvm-link $? -o klee_wrapper.bc

main_klee.bc rbtree.bc: main_klee.c rbtree.c
	clang -I linux -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone $^

.PHONY:
klee_gcov: main_klee.c rbtree.c
	@gcc -I linux -fprofile-arcs -ftest-coverage $^ -o klee_wrapper
	@for test in $(TEST_DIR)/*.stdin ; do ./klee_wrapper < "$$test"; done
	@gcov *.gcda

.PHONY:
clean:
	@$(RM) *.bc *.gcno *.gcov *.gcda klee_wrapper harness_wrapper
