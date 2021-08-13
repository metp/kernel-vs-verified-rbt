#!/bin/sh

function rebuild {
	make clean > /dev/null
	make cov > /dev/null
}

function analyze {
	rebuild

	echo random strategy
	rbtTestHarness random -pn 50000 | ./harness_wrapper$1
	gcov harness_wrapper$1-rbtree.gcda | head -n 2 | tail -n 1

	rebuild

	echo "exhaustive strategy (this may take some seconds)"
	rbtTestHarness exhaustive -pn 9 | ./harness_wrapper$1
	gcov harness_wrapper$1-rbtree.gcda | head -n 2 | tail -n 1

	rebuild

	echo symbolic strategy
	for test in klee-out_STDIN_SIZE=14/*.stdin;
	do
		./klee_wrapper$1 < "$test";
	done
	gcov klee_wrapper$1-rbtree.gcda | head -n 2 | tail -n 1
}

echo "##### Coverage of rbtree.c (core+auxiliary) #####"
echo
analyze
echo
echo "##### Coverage of rbtree.c (core) #####"
echo
analyze _core
