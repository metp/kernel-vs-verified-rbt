(* Author: Tobias Nipkow *)

section \<open>Red-Black Tree Implementation of Sets\<close>

theory RBT_Set
imports
  Complex_Main
  RBT
  "HOL-Data_Structures.Cmp"
  "HOL-Data_Structures.Isin2"
begin

definition empty :: "'a rbt" where
"empty = Leaf"

fun ins :: "'a::linorder \<Rightarrow> 'a rbt \<Rightarrow> 'a rbt" where
"ins x Leaf = R Leaf x Leaf" |
"ins x (B l a r) =
  (case cmp x a of
     LT \<Rightarrow> baliL (ins x l) a r |
     GT \<Rightarrow> baliR l a (ins x r) |
     EQ \<Rightarrow> B l a r)" |
"ins x (R l a r) =
  (case cmp x a of
    LT \<Rightarrow> R (ins x l) a r |
    GT \<Rightarrow> R l a (ins x r) |
    EQ \<Rightarrow> R l a r)"

definition insert :: "'a::linorder \<Rightarrow> 'a rbt \<Rightarrow> 'a rbt" where
"insert x t = paint Black (ins x t)"

fun color :: "'a rbt \<Rightarrow> color" where
"color Leaf = Black" |
"color (Node _ (_, c) _) = c"

subsection "Functional Correctness Proofs"

lemma inorder_paint: "inorder(paint c t) = inorder t"
by(cases t) (auto)

lemma inorder_baliL:
  "inorder(baliL l a r) = inorder l @ a # inorder r"
by(cases "(l,a,r)" rule: baliL.cases) (auto)

lemma inorder_baliR:
  "inorder(baliR l a r) = inorder l @ a # inorder r"
by(cases "(l,a,r)" rule: baliR.cases) (auto)

lemma inorder_ins:
  "sorted(inorder t) \<Longrightarrow> inorder(ins x t) = ins_list x (inorder t)"
by(induction x t rule: ins.induct)
  (auto simp: ins_list_simps inorder_baliL inorder_baliR)

lemma inorder_insert:
  "sorted(inorder t) \<Longrightarrow> inorder(insert x t) = ins_list x (inorder t)"
by (simp add: insert_def inorder_ins inorder_paint)


subsection \<open>Structural invariants\<close>

lemma neq_Black[simp]: "(c \<noteq> Black) = (c = Red)"
by (cases c) auto

text\<open>The proofs are due to Markus Reiter and Alexander Krauss.\<close>
text\<open>The Linux RBT insert proof modification is due to Mete Polat.\<close>

fun bheight :: "'a rbt \<Rightarrow> nat" where
"bheight Leaf = 0" |
"bheight (Node l (x, c) r) = (if c = Black then bheight l + 1 else bheight l)"

fun invc :: "'a rbt \<Rightarrow> bool" where
"invc Leaf = True" |
"invc (Node l (a,c) r) =
  ((c = Red \<longrightarrow> color l = Black \<and> color r = Black) \<and> invc l \<and> invc r)"

text \<open>Weaker versions:\<close>
fun invc2 :: "'a rbt \<Rightarrow> bool" where
"invc2 \<langle>\<rangle> = True" |
"invc2 (Node l (_,c) r) = ((c = Red \<longrightarrow> (color l = Black) \<or> (color r = Black)) \<and> invc l \<and> invc r)"

abbreviation invc3 :: "'a rbt \<Rightarrow> bool" where
"invc3 t \<equiv> invc(paint Black t)"

fun invh :: "'a rbt \<Rightarrow> bool" where
"invh Leaf = True" |
"invh (Node l (x, c) r) = (bheight l = bheight r \<and> invh l \<and> invh r)"

lemma invc2I: "invc t \<Longrightarrow> invc2 t"
  by (cases t rule: tree2_cases) simp+

lemma invc3I: "invc t \<Longrightarrow> invc3 t"
  by (cases t rule: tree2_cases) simp+

lemma invc23I: "invc2 t \<Longrightarrow> invc3 t"
  by (cases t rule: tree2_cases) simp+

definition rbt :: "'a rbt \<Rightarrow> bool" where
"rbt t = (invc t \<and> invh t \<and> color t = Black)"

lemma color_paint_Black: "color (paint Black t) = Black"
by (cases t) auto

lemma paint2: "paint c2 (paint c1 t) = paint c2 t"
by (cases t) auto

lemma invh_paint: "invh t \<Longrightarrow> invh (paint c t)"
by (cases t) auto

lemma invc_baliL:
  "\<lbrakk>invc2 l; invc r\<rbrakk> \<Longrightarrow> invc (baliL l a r)" 
  by (induct l a r rule: baliL.induct) auto

lemma invc_baliR:
  "\<lbrakk>invc l; invc2 r\<rbrakk> \<Longrightarrow> invc (baliR l a r)" 
by (induct l a r rule: baliR.induct) auto

lemma bheight_baliL:
  "bheight l = bheight r \<Longrightarrow> bheight (baliL l a r) = Suc (bheight l)"
by (induct l a r rule: baliL.induct) auto

lemma bheight_baliR:
  "bheight l = bheight r \<Longrightarrow> bheight (baliR l a r) = Suc (bheight l)"
by (induct l a r rule: baliR.induct) auto

lemma invh_baliL: 
  "\<lbrakk> invh l; invh r; bheight l = bheight r \<rbrakk> \<Longrightarrow> invh (baliL l a r)"
by (induct l a r rule: baliL.induct) auto

lemma invh_baliR: 
  "\<lbrakk> invh l; invh r; bheight l = bheight r \<rbrakk> \<Longrightarrow> invh (baliR l a r)"
by (induct l a r rule: baliR.induct) auto

lemma ins_Red_invcs: "\<lbrakk>invc t; invc3 (ins x t); color t = Red\<rbrakk> \<Longrightarrow> invc2 (ins x t)"
  by (induct x t rule: ins.induct) auto

text \<open>All in one:\<close>

lemma inv_baliR: "\<lbrakk> invh l; invh r; invc l; invc2 r; bheight l = bheight r \<rbrakk>
 \<Longrightarrow> invc (baliR l a r) \<and> invh (baliR l a r) \<and> bheight (baliR l a r) = Suc (bheight l)"
by (induct l a r rule: baliR.induct) auto

lemma inv_baliL: "\<lbrakk> invh l; invh r; invc2 l; invc r; bheight l = bheight r \<rbrakk>
 \<Longrightarrow> invc (baliL l a r) \<and> invh (baliL l a r) \<and> bheight (baliL l a r) = Suc (bheight l)"
by (induct l a r rule: baliL.induct) auto

subsubsection \<open>Insertion\<close>

lemma invc_ins: "invc t \<Longrightarrow> invc3 (ins x t) \<and> (color t = Black \<longrightarrow> invc (ins x t))"
  by (induct x t rule: ins.induct) (auto simp: invc_baliL invc_baliR invc2I invc3I ins_Red_invcs)

lemma invh_ins: "invh t \<Longrightarrow> invh (ins x t) \<and> bheight (ins x t) = bheight t"
by(induct x t rule: ins.induct)
  (auto simp: invh_baliL invh_baliR bheight_baliL bheight_baliR)

theorem rbt_insert: "rbt t \<Longrightarrow> rbt (insert x t)"
by (simp add: invc_ins invh_ins color_paint_Black invh_paint rbt_def insert_def)

text \<open>All in one:\<close>

lemma inv_ins: "\<lbrakk> invc t; invh t \<rbrakk> \<Longrightarrow>
  invc2 (ins x t) \<and> (color t = Black \<longrightarrow> invc (ins x t)) \<and>
  invh(ins x t) \<and> bheight (ins x t) = bheight t"
by (induct x t rule: ins.induct) (auto simp: inv_baliL inv_baliR invc2I)

theorem rbt_insert2: "rbt t \<Longrightarrow> rbt (insert x t)"
  by (simp add: inv_ins color_paint_Black invh_paint rbt_def insert_def invc3I ins_Red_invcs)

text \<open>Overall correctness:\<close>

interpretation S: Set_by_Ordered
where empty = empty and isin = isin and insert = insert and delete = delete
and inorder = inorder and inv = rbt
proof (standard, goal_cases)
  case 1 show ?case by (simp add: empty_def)
next
  case 2 thus ?case by(simp add: isin_set_inorder)
next
  case 3 thus ?case by(simp add: inorder_insert)
next
  (* Linux RBT delete not formalized *)
  case 4 thus ?case sorry
next
  case 5 thus ?case by (simp add: rbt_def empty_def) 
next
  case 6 thus ?case by (simp add: rbt_insert) 
next
  (* Linux RBT delete not formalized *)
  case 7 thus ?case sorry
qed


subsection \<open>Height-Size Relation\<close>

lemma rbt_height_bheight_if: "invc t \<Longrightarrow> invh t \<Longrightarrow>
  height t \<le> 2 * bheight t + (if color t = Black then 0 else 1)"
by(induction t) (auto split: if_split_asm)

lemma rbt_height_bheight: "rbt t \<Longrightarrow> height t / 2 \<le> bheight t "
by(auto simp: rbt_def dest: rbt_height_bheight_if)

lemma bheight_size_bound:  "invc t \<Longrightarrow> invh t \<Longrightarrow> 2 ^ (bheight t) \<le> size1 t"
by (induction t) auto

lemma rbt_height_le: assumes "rbt t" shows "height t \<le> 2 * log 2 (size1 t)"
proof -
  have "2 powr (height t / 2) \<le> 2 powr bheight t"
    using rbt_height_bheight[OF assms] by (simp)
  also have "\<dots> \<le> size1 t" using assms
    by (simp add: powr_realpow bheight_size_bound rbt_def)
  finally have "2 powr (height t / 2) \<le> size1 t" .
  hence "height t / 2 \<le> log 2 (size1 t)"
    by (simp add: le_log_iff size1_size del: divide_le_eq_numeral1(1))
  thus ?thesis by simp
qed

end
