(* Author: Tobias Nipkow
   Author: Mete Polat *)

section \<open>Red-Black Trees\<close>

theory RBT
imports "HOL-Data_Structures.Tree2"
begin

datatype color = Red | Black

type_synonym 'a rbt = "('a*color)tree"

abbreviation R where "R l a r \<equiv> Node l (a, Red) r"
abbreviation B where "B l a r \<equiv> Node l (a, Black) r"

fun baliL :: "'a rbt \<Rightarrow> 'a \<Rightarrow> 'a rbt \<Rightarrow> 'a rbt" where
"baliL (R (R t1 n t2) p t3) g (R t4 u t5) = R (B (R t1 n t2) p t3) g (B t4 u t5)" |
"baliL (R t1 p (R t2 n t3)) g (R t4 u t5) = R (B t1 p (R t2 n t3)) g (B t4 u t5)" |

"baliL (R t1 p (R t2 n t3)) g t4 = B (R t1 p t2) n (R t3 g t4)" |
"baliL (R (R t1 n t2) p t3) g t4 = B (R t1 n t2) p (R t3 g t4)" |

"baliL t1 a t2 = B t1 a t2"

fun baliR :: "'a rbt \<Rightarrow> 'a \<Rightarrow> 'a rbt \<Rightarrow> 'a rbt" where
"baliR (R t1 u t2) g (R t3 p (R t4 n t5)) = R (B t1 u t2) g (B t3 p (R t4 n t5))" |
"baliR (R t1 u t2) g (R (R t3 n t4) p t5) = R (B t1 u t2) g (B (R t3 n t4) p t5)" |

"baliR t1 g (R (R t2 n t3) p t4) = B (R t1 g t2) n (R t3 p t4)" |
"baliR t1 g (R t2 p (R t3 n t4)) = B (R t1 g t2) p (R t3 n t4)" |

"baliR t1 a t2 = B t1 a t2"


fun paint :: "color \<Rightarrow> 'a rbt \<Rightarrow> 'a rbt" where
"paint c Leaf = Leaf" |
"paint c (Node l (a,_) r) = Node l (a,c) r"

end
