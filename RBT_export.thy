theory RBT_export  
imports
  RBT
  RBT_Set
  "HOL-Data_Structures.Isin2"
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

(*| class_instance integer :: linorder \<rightharpoonup> (Haskell) -*)

export_code
  RBT_Set.empty
  RBT_Set.insert
  RBT_Set.delete
  Isin2.isin
in Haskell module_name RBT.Verified (string_classes)

end