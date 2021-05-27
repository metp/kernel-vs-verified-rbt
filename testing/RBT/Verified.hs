{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  RBT.Verified(Color, Nat, Tree, invc, invh, empty, inorder, delete, insert,
                rootBlack, rbt)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;

data Color = R | B deriving (Prelude.Read, Prelude.Show);

equal_color :: Color -> Color -> Bool;
equal_color R B = False;
equal_color B R = False;
equal_color B B = True;
equal_color R R = True;

instance Eq Color where {
  a == b = equal_color a b;
};

newtype Nat = Nat Integer deriving (Prelude.Read, Prelude.Show);

data Num = One | Bit0 Num | Bit1 Num deriving (Prelude.Read, Prelude.Show);

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Prelude.Read, Prelude.Show);

data Cmp_val = LT | EQ | GT deriving (Prelude.Read, Prelude.Show);

cmp :: forall a. (Eq a, Prelude.Ord a) => a -> a -> Cmp_val;
cmp x y = (if x < y then LT else (if x == y then EQ else GT));

paint :: forall a. Color -> Tree (a, Color) -> Tree (a, Color);
paint c Leaf = Leaf;
paint c (Node l (a, uu) r) = Node l (a, c) r;

baliR :: forall a. Tree (a, Color) -> a -> Tree (a, Color) -> Tree (a, Color);
baliR t1 a (Node t2 (b, R) (Node t3 (c, R) t4)) =
  Node (Node t1 (a, B) t2) (b, R) (Node t3 (c, B) t4);
baliR t1 a (Node (Node t2 (b, R) t3) (c, R) Leaf) =
  Node (Node t1 (a, B) t2) (b, R) (Node t3 (c, B) Leaf);
baliR t1 a (Node (Node t2 (b, R) t3) (c, R) (Node v (vc, B) vb)) =
  Node (Node t1 (a, B) t2) (b, R)
    (Node t3 (c, B) (Node v (vc, B) vb));
baliR t1 a Leaf = Node t1 (a, B) Leaf;
baliR t1 a (Node v (vc, B) vb) = Node t1 (a, B) (Node v (vc, B) vb);
baliR t1 a (Node Leaf va Leaf) = Node t1 (a, B) (Node Leaf va Leaf);
baliR t1 a (Node (Node vb (ve, B) vd) va Leaf) =
  Node t1 (a, B) (Node (Node vb (ve, B) vd) va Leaf);
baliR t1 a (Node Leaf va (Node vc (vf, B) ve)) =
  Node t1 (a, B) (Node Leaf va (Node vc (vf, B) ve));
baliR t1 a (Node (Node vb (vh, B) vg) va (Node vc (vf, B) ve)) =
  Node t1 (a, B)
    (Node (Node vb (vh, B) vg) va (Node vc (vf, B) ve));

baldL :: forall a. Tree (a, Color) -> a -> Tree (a, Color) -> Tree (a, Color);
baldL (Node t1 (a, R) t2) b t3 = Node (Node t1 (a, B) t2) (b, R) t3;
baldL Leaf a (Node t2 (b, B) t3) = baliR Leaf a (Node t2 (b, R) t3);
baldL (Node v (vc, B) vb) a (Node t2 (b, B) t3) =
  baliR (Node v (vc, B) vb) a (Node t2 (b, R) t3);
baldL Leaf a (Node (Node t2 (b, B) t3) (c, R) t4) =
  Node (Node Leaf (a, B) t2) (b, R) (baliR t3 c (paint R t4));
baldL (Node v (vc, B) vb) a (Node (Node t2 (b, B) t3) (c, R) t4) =
  Node (Node (Node v (vc, B) vb) (a, B) t2) (b, R)
    (baliR t3 c (paint R t4));
baldL Leaf a Leaf = Node Leaf (a, R) Leaf;
baldL Leaf a (Node Leaf (vc, R) vb) =
  Node Leaf (a, R) (Node Leaf (vc, R) vb);
baldL Leaf a (Node (Node va (vf, R) ve) (vc, R) vb) =
  Node Leaf (a, R) (Node (Node va (vf, R) ve) (vc, R) vb);
baldL (Node v (vc, B) vb) a Leaf =
  Node (Node v (vc, B) vb) (a, R) Leaf;
baldL (Node v (vc, B) vb) a (Node Leaf (vf, R) ve) =
  Node (Node v (vc, B) vb) (a, R) (Node Leaf (vf, R) ve);
baldL (Node v (vc, B) vb) a (Node (Node vd (vi, R) vh) (vf, R) ve) =
  Node (Node v (vc, B) vb) (a, R)
    (Node (Node vd (vi, R) vh) (vf, R) ve);

join :: forall a. Tree (a, Color) -> Tree (a, Color) -> Tree (a, Color);
join Leaf t = t;
join (Node v va vb) Leaf = Node v va vb;
join (Node t1 (a, R) t2) (Node t3 (c, R) t4) =
  (case join t2 t3 of {
    Leaf -> Node t1 (a, R) (Node Leaf (c, R) t4);
    Node u2 (b, R) u3 ->
      Node (Node t1 (a, R) u2) (b, R) (Node u3 (c, R) t4);
    Node u2 (b, B) u3 ->
      Node t1 (a, R) (Node (Node u2 (b, B) u3) (c, R) t4);
  });
join (Node t1 (a, B) t2) (Node t3 (c, B) t4) =
  (case join t2 t3 of {
    Leaf -> baldL t1 a (Node Leaf (c, B) t4);
    Node u2 (b, R) u3 ->
      Node (Node t1 (a, B) u2) (b, R) (Node u3 (c, B) t4);
    Node u2 (b, B) u3 ->
      baldL t1 a (Node (Node u2 (b, B) u3) (c, B) t4);
  });
join (Node v (vc, B) vb) (Node t2 (a, R) t3) =
  Node (join (Node v (vc, B) vb) t2) (a, R) t3;
join (Node t1 (a, R) t2) (Node v (vc, B) vb) =
  Node t1 (a, R) (join t2 (Node v (vc, B) vb));

baliL :: forall a. Tree (a, Color) -> a -> Tree (a, Color) -> Tree (a, Color);
baliL (Node (Node t1 (a, R) t2) (b, R) t3) c t4 =
  Node (Node t1 (a, B) t2) (b, R) (Node t3 (c, B) t4);
baliL (Node Leaf (a, R) (Node t2 (b, R) t3)) c t4 =
  Node (Node Leaf (a, B) t2) (b, R) (Node t3 (c, B) t4);
baliL (Node (Node v (vc, B) vb) (a, R) (Node t2 (b, R) t3)) c t4 =
  Node (Node (Node v (vc, B) vb) (a, B) t2) (b, R)
    (Node t3 (c, B) t4);
baliL Leaf a t2 = Node Leaf (a, B) t2;
baliL (Node Leaf (v, B) vb) a t2 =
  Node (Node Leaf (v, B) vb) (a, B) t2;
baliL (Node Leaf va Leaf) a t2 = Node (Node Leaf va Leaf) (a, B) t2;
baliL (Node Leaf va (Node v (ve, B) vd)) a t2 =
  Node (Node Leaf va (Node v (ve, B) vd)) (a, B) t2;
baliL (Node (Node vc (vf, B) ve) (v, B) vb) a t2 =
  Node (Node (Node vc (vf, B) ve) (v, B) vb) (a, B) t2;
baliL (Node (Node vc (vf, B) ve) va Leaf) a t2 =
  Node (Node (Node vc (vf, B) ve) va Leaf) (a, B) t2;
baliL (Node (Node vc (vf, B) ve) va (Node v (vh, B) vg)) a t2 =
  Node (Node (Node vc (vf, B) ve) va (Node v (vh, B) vg)) (a, B) t2;
baliL (Node v (vc, B) vb) a t2 = Node (Node v (vc, B) vb) (a, B) t2;

baldR :: forall a. Tree (a, Color) -> a -> Tree (a, Color) -> Tree (a, Color);
baldR t1 a (Node t2 (b, R) t3) = Node t1 (a, R) (Node t2 (b, B) t3);
baldR (Node t1 (a, B) t2) b Leaf = baliL (Node t1 (a, R) t2) b Leaf;
baldR (Node t1 (a, B) t2) b (Node v (vc, B) vb) =
  baliL (Node t1 (a, R) t2) b (Node v (vc, B) vb);
baldR (Node t1 (a, R) (Node t2 (b, B) t3)) c Leaf =
  Node (baliL (paint R t1) a t2) (b, R) (Node t3 (c, B) Leaf);
baldR (Node t1 (a, R) (Node t2 (b, B) t3)) c (Node v (vc, B) vb) =
  Node (baliL (paint R t1) a t2) (b, R)
    (Node t3 (c, B) (Node v (vc, B) vb));
baldR Leaf a Leaf = Node Leaf (a, R) Leaf;
baldR (Node v (vc, R) Leaf) a Leaf =
  Node (Node v (vc, R) Leaf) (a, R) Leaf;
baldR (Node v (vc, R) (Node va (vf, R) ve)) a Leaf =
  Node (Node v (vc, R) (Node va (vf, R) ve)) (a, R) Leaf;
baldR Leaf a (Node v (vc, B) vb) =
  Node Leaf (a, R) (Node v (vc, B) vb);
baldR (Node va (vf, R) Leaf) a (Node v (vc, B) vb) =
  Node (Node va (vf, R) Leaf) (a, R) (Node v (vc, B) vb);
baldR (Node va (vf, R) (Node vd (vi, R) vh)) a (Node v (vc, B) vb) =
  Node (Node va (vf, R) (Node vd (vi, R) vh)) (a, R)
    (Node v (vc, B) vb);

equal_tree :: forall a. (Eq a) => Tree a -> Tree a -> Bool;
equal_tree Leaf (Node x21 x22 x23) = False;
equal_tree (Node x21 x22 x23) Leaf = False;
equal_tree (Node x21 x22 x23) (Node y21 y22 y23) =
  equal_tree x21 y21 && x22 == y22 && equal_tree x23 y23;
equal_tree Leaf Leaf = True;

color :: forall a. Tree (a, Color) -> Color;
color Leaf = B;
color (Node uu (uv, c) uw) = c;

del ::
  forall a. (Eq a, Prelude.Ord a) => a -> Tree (a, Color) -> Tree (a, Color);
del x Leaf = Leaf;
del x (Node l (a, uu) r) =
  (case cmp x a of {
    LT -> (if not (equal_tree l Leaf) && equal_color (color l) B
            then baldL (del x l) a r else Node (del x l) (a, R) r);
    EQ -> join l r;
    GT -> (if not (equal_tree r Leaf) && equal_color (color r) B
            then baldR l a (del x r) else Node l (a, R) (del x r));
  });

ins ::
  forall a. (Eq a, Prelude.Ord a) => a -> Tree (a, Color) -> Tree (a, Color);
ins x Leaf = Node Leaf (x, R) Leaf;
ins x (Node l (a, B) r) = (case cmp x a of {
                                LT -> baliL (ins x l) a r;
                                EQ -> Node l (a, B) r;
                                GT -> baliR l a (ins x r);
                              });
ins x (Node l (a, R) r) = (case cmp x a of {
                              LT -> Node (ins x l) (a, R) r;
                              EQ -> Node l (a, R) r;
                              GT -> Node l (a, R) (ins x r);
                            });

invc :: forall a. Tree (a, Color) -> Bool;
invc Leaf = True;
invc (Node l (a, c) r) =
  (if equal_color c R
    then equal_color (color l) B && equal_color (color r) B
    else True) &&
    invc l && invc r;

integer_of_nat :: Nat -> Integer;
integer_of_nat (Nat x) = x;

equal_nat :: Nat -> Nat -> Bool;
equal_nat m n = integer_of_nat m == integer_of_nat n;

zero_nat :: Nat;
zero_nat = Nat (0 :: Integer);

plus_nat :: Nat -> Nat -> Nat;
plus_nat m n = Nat (integer_of_nat m + integer_of_nat n);

one_nat :: Nat;
one_nat = Nat (1 :: Integer);

bheight :: forall a. Tree (a, Color) -> Nat;
bheight Leaf = zero_nat;
bheight (Node l (x, c) r) =
  (if equal_color c B then plus_nat (bheight l) one_nat else bheight l);

invh :: forall a. Tree (a, Color) -> Bool;
invh Leaf = True;
invh (Node l (x, c) r) = equal_nat (bheight l) (bheight r) && invh l && invh r;

empty :: forall a. Tree (a, Color);
empty = Leaf;

inorder :: forall a b. Tree (a, b) -> [a];
inorder Leaf = [];
inorder (Node l (a, uu) r) = inorder l ++ a : inorder r;

delete ::
  forall a. (Eq a, Prelude.Ord a) => a -> Tree (a, Color) -> Tree (a, Color);
delete x t = paint B (del x t);

insert ::
  forall a. (Eq a, Prelude.Ord a) => a -> Tree (a, Color) -> Tree (a, Color);
insert x t = paint B (ins x t);

rootBlack :: forall a. Tree (a, Color) -> Bool;
rootBlack t = equal_color (color t) B;

rbt :: forall a. Tree (a, Color) -> Bool;
rbt t = invc t && invh t && rootBlack t;

}
