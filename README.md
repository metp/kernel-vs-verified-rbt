# Source Code of my Bachelor's Thesis: Testing the Red-Black Tree Implementation of the Linux Kernel against a Formally Verified Variant
## Abstract
This thesis shows how to construct evidence of correctness for the Red-Black tree (RBT)
implementation of the Linux kernel through testing and formal verification. First, it
describes how to verify the results of kernel RBT operations by comparing them against
the results of a formally verified RBT variant. Second, it shows how modifying this
verified implementation and identifying new invariants makes it possible to prove the
correctness of the kernel RBT insert algorithm in Isabelle/HOL.

## Structure
* [module](https://github.com/metp/kernel-vs-verified-rbt/tree/main/module) hosts the Linux kernel module.
* [harness](https://github.com/metp/kernel-vs-verified-rbt/tree/main/harness) hosts the testing harness together with the verified RBT Haskell variant.
* [HOL-RBT](https://github.com/metp/kernel-vs-verified-rbt/tree/main/HOL-RBT) hosts the Isabelle/HOL formalization and proof of the Kernel RBT insert routine.
It also includes an Isabelle file to export the original verified Isabelle/HOL RBT to Haskell.
* [userspaceRBT](https://github.com/metp/kernel-vs-verified-rbt/tree/main/userspaceRBT) hosts the modified kernel RBT implementation that I lifted to userspace,
 a Makefile to start a symbolic execution with [KLEE](https://github.com/klee/klee) using `make symbolic STDIN_SIZE=<N>`
 (where `<N>` is even; requires KLEE & LLVM). In the thesis' evaluation I used `N=14`. The final test cases of this execution are in
 [userspaceRBT/klee-out_STDIN_SIZE=14](https://github.com/metp/kernel-vs-verified-rbt/tree/main/userspaceRBT/klee-out_STDIN_SIZE%3D14).
 `cov.sh` reproduces the coverage data shown in the thesis.
 
