theory RBT_export  
imports
  "HOL-Data_Structures.RBT_Set"
  "HOL-Data_Structures.Tree2"
  "HOL-Library.Code_Target_Numeral"
  HOL.Orderings
begin

code_printing
(* This mapping is mathematically correct *)
  type_class linorder \<rightharpoonup> (Haskell) "Prelude.Ord"
(* 
This one not as it claims that all orderings are linear.
However that's fine because all ordered types in the RBT interface are linear orders *)
| type_class ord \<rightharpoonup> (Haskell) "Prelude.Ord"
| constant Orderings.less \<rightharpoonup> (Haskell) infixl 4 "<"
| constant Orderings.less_eq \<rightharpoonup> (Haskell) infixl 4 "<="
| type_class order \<rightharpoonup> (Haskell) "Prelude.Ord"
| type_class preorder \<rightharpoonup> (Haskell) "Prelude.Ord"

definition rootBlack :: "'a rbt \<Rightarrow> bool" where
"rootBlack t \<equiv> color t = Black"

definition rbt :: "'a rbt \<Rightarrow> bool" where
"rbt t \<equiv> invc t \<and> invh t \<and> rootBlack t"

lemma "RBT_Set.rbt t \<equiv> rbt t"
  by (simp add: RBT_Set.rbt_def rbt_def rootBlack_def)

definition isEmpty :: "'a rbt \<Rightarrow> bool" where
"isEmpty t \<equiv> t = Leaf"

export_code
  rbt
  RBT_Set.invc
  RBT_Set.invh
  rootBlack
  isEmpty
  RBT_Set.empty
  RBT_Set.insert
  RBT_Set.delete
  Tree2.inorder
in Haskell module_name RBT.Verified (string_classes)

end
