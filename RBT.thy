(* Author: Tobias Nipkow *)

section \<open>Red-Black Trees\<close>

theory RBT
imports "HOL-Data_Structures.Tree2"
begin

datatype color = Red | Black

type_synonym 'a rbt = "('a*color)tree"

abbreviation R where "R l a r \<equiv> Node l (a, Red) r"
abbreviation B where "B l a r \<equiv> Node l (a, Black) r"

(*

"baliL (R t1 p (R t2 n t3)) g (B t4 u t5) = B (R t1 p t2) n (R t3 g (B t4 u t5))" |
"baliL (R t1 p (R t2 n t3)) g Leaf = B (R t1 p t2) n (R t3 g Leaf)" |

"baliR (B t1 u t2) g (R (R t3 n t4) p t5) = B (R (B t1 u t2) g t3) n (R t4 p t5)" |
"baliR Leaf g (R (R t3 n t4) p t5) = B (R Leaf g t3) n (R t4 p t5)" |

*)

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

fun baldL :: "'a rbt \<Rightarrow> 'a \<Rightarrow> 'a rbt \<Rightarrow> 'a rbt" where
"baldL (R t1 a t2) b t3 = R (B t1 a t2) b t3" |
"baldL t1 a (B t2 b t3) = baliR t1 a (R t2 b t3)" |
"baldL t1 a (R (B t2 b t3) c t4) = R (B t1 a t2) b (baliR t3 c (paint Red t4))" |
"baldL t1 a t2 = R t1 a t2"

fun baldR :: "'a rbt \<Rightarrow> 'a \<Rightarrow> 'a rbt \<Rightarrow> 'a rbt" where
"baldR t1 a (R t2 b t3) = R t1 a (B t2 b t3)" |
"baldR (B t1 a t2) b t3 = baliL (R t1 a t2) b t3" |
"baldR (R t1 a (B t2 b t3)) c t4 = R (baliL (paint Red t1) a t2) b (B t3 c t4)" |
"baldR t1 a t2 = R t1 a t2"

fun join :: "'a rbt \<Rightarrow> 'a rbt \<Rightarrow> 'a rbt" where
"join Leaf t = t" |
"join t Leaf = t" |
"join (R t1 a t2) (R t3 c t4) =
  (case join t2 t3 of
     R u2 b u3 \<Rightarrow> (R (R t1 a u2) b (R u3 c t4)) |
     t23 \<Rightarrow> R t1 a (R t23 c t4))" |
"join (B t1 a t2) (B t3 c t4) =
  (case join t2 t3 of
     R u2 b u3 \<Rightarrow> R (B t1 a u2) b (B u3 c t4) |
     t23 \<Rightarrow> baldL t1 a (B t23 c t4))" |
"join t1 (R t2 a t3) = R (join t1 t2) a t3" |
"join (R t1 a t2) t3 = R t1 a (join t2 t3)" 

end
