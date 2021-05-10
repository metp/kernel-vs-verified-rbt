{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module RBT.Verified(Rbta, Rbt, empty, delete, insert, lookup, height, height_balanced) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;

data Color = R | B deriving (Prelude.Read, Prelude.Show);

data Rbta a b = Empty | Branch Color (Rbta a b) a b (Rbta a b)
  deriving (Prelude.Read, Prelude.Show);

newtype Rbt b a = RBT (Rbta b a) deriving (Prelude.Read, Prelude.Show);

empty :: forall a b. (Prelude.Ord a) => Rbt a b;
empty = RBT Empty;

balance :: forall a b. Rbta a b -> a -> b -> Rbta a b -> Rbta a b;
balance (Branch R a w x b) s t (Branch R c y z d) =
  Branch R (Branch B a w x b) s t (Branch B c y z d);
balance (Branch R (Branch R a w x b) s t c) y z Empty =
  Branch R (Branch B a w x b) s t (Branch B c y z Empty);
balance (Branch R (Branch R a w x b) s t c) y z (Branch B va vb vc vd) =
  Branch R (Branch B a w x b) s t (Branch B c y z (Branch B va vb vc vd));
balance (Branch R Empty w x (Branch R b s t c)) y z Empty =
  Branch R (Branch B Empty w x b) s t (Branch B c y z Empty);
balance (Branch R (Branch B va vb vc vd) w x (Branch R b s t c)) y z Empty =
  Branch R (Branch B (Branch B va vb vc vd) w x b) s t (Branch B c y z Empty);
balance (Branch R Empty w x (Branch R b s t c)) y z (Branch B va vb vc vd) =
  Branch R (Branch B Empty w x b) s t (Branch B c y z (Branch B va vb vc vd));
balance (Branch R (Branch B ve vf vg vh) w x (Branch R b s t c)) y z
  (Branch B va vb vc vd) =
  Branch R (Branch B (Branch B ve vf vg vh) w x b) s t
    (Branch B c y z (Branch B va vb vc vd));
balance Empty w x (Branch R b s t (Branch R c y z d)) =
  Branch R (Branch B Empty w x b) s t (Branch B c y z d);
balance (Branch B va vb vc vd) w x (Branch R b s t (Branch R c y z d)) =
  Branch R (Branch B (Branch B va vb vc vd) w x b) s t (Branch B c y z d);
balance Empty w x (Branch R (Branch R b s t c) y z Empty) =
  Branch R (Branch B Empty w x b) s t (Branch B c y z Empty);
balance Empty w x (Branch R (Branch R b s t c) y z (Branch B va vb vc vd)) =
  Branch R (Branch B Empty w x b) s t (Branch B c y z (Branch B va vb vc vd));
balance (Branch B va vb vc vd) w x (Branch R (Branch R b s t c) y z Empty) =
  Branch R (Branch B (Branch B va vb vc vd) w x b) s t (Branch B c y z Empty);
balance (Branch B va vb vc vd) w x
  (Branch R (Branch R b s t c) y z (Branch B ve vf vg vh)) =
  Branch R (Branch B (Branch B va vb vc vd) w x b) s t
    (Branch B c y z (Branch B ve vf vg vh));
balance Empty s t Empty = Branch B Empty s t Empty;
balance Empty s t (Branch B va vb vc vd) =
  Branch B Empty s t (Branch B va vb vc vd);
balance Empty s t (Branch v Empty vb vc Empty) =
  Branch B Empty s t (Branch v Empty vb vc Empty);
balance Empty s t (Branch v (Branch B ve vf vg vh) vb vc Empty) =
  Branch B Empty s t (Branch v (Branch B ve vf vg vh) vb vc Empty);
balance Empty s t (Branch v Empty vb vc (Branch B vf vg vh vi)) =
  Branch B Empty s t (Branch v Empty vb vc (Branch B vf vg vh vi));
balance Empty s t (Branch v (Branch B ve vj vk vl) vb vc (Branch B vf vg vh vi))
  = Branch B Empty s t
      (Branch v (Branch B ve vj vk vl) vb vc (Branch B vf vg vh vi));
balance (Branch B va vb vc vd) s t Empty =
  Branch B (Branch B va vb vc vd) s t Empty;
balance (Branch B va vb vc vd) s t (Branch B ve vf vg vh) =
  Branch B (Branch B va vb vc vd) s t (Branch B ve vf vg vh);
balance (Branch B va vb vc vd) s t (Branch v Empty vf vg Empty) =
  Branch B (Branch B va vb vc vd) s t (Branch v Empty vf vg Empty);
balance (Branch B va vb vc vd) s t (Branch v (Branch B vi vj vk vl) vf vg Empty)
  = Branch B (Branch B va vb vc vd) s t
      (Branch v (Branch B vi vj vk vl) vf vg Empty);
balance (Branch B va vb vc vd) s t (Branch v Empty vf vg (Branch B vj vk vl vm))
  = Branch B (Branch B va vb vc vd) s t
      (Branch v Empty vf vg (Branch B vj vk vl vm));
balance (Branch B va vb vc vd) s t
  (Branch v (Branch B vi vn vo vp) vf vg (Branch B vj vk vl vm)) =
  Branch B (Branch B va vb vc vd) s t
    (Branch v (Branch B vi vn vo vp) vf vg (Branch B vj vk vl vm));
balance (Branch v Empty vb vc Empty) s t Empty =
  Branch B (Branch v Empty vb vc Empty) s t Empty;
balance (Branch v Empty vb vc (Branch B ve vf vg vh)) s t Empty =
  Branch B (Branch v Empty vb vc (Branch B ve vf vg vh)) s t Empty;
balance (Branch v (Branch B vf vg vh vi) vb vc Empty) s t Empty =
  Branch B (Branch v (Branch B vf vg vh vi) vb vc Empty) s t Empty;
balance (Branch v (Branch B vf vg vh vi) vb vc (Branch B ve vj vk vl)) s t Empty
  = Branch B (Branch v (Branch B vf vg vh vi) vb vc (Branch B ve vj vk vl)) s t
      Empty;
balance (Branch v Empty vf vg Empty) s t (Branch B va vb vc vd) =
  Branch B (Branch v Empty vf vg Empty) s t (Branch B va vb vc vd);
balance (Branch v Empty vf vg (Branch B vi vj vk vl)) s t (Branch B va vb vc vd)
  = Branch B (Branch v Empty vf vg (Branch B vi vj vk vl)) s t
      (Branch B va vb vc vd);
balance (Branch v (Branch B vj vk vl vm) vf vg Empty) s t (Branch B va vb vc vd)
  = Branch B (Branch v (Branch B vj vk vl vm) vf vg Empty) s t
      (Branch B va vb vc vd);
balance (Branch v (Branch B vj vk vl vm) vf vg (Branch B vi vn vo vp)) s t
  (Branch B va vb vc vd) =
  Branch B (Branch v (Branch B vj vk vl vm) vf vg (Branch B vi vn vo vp)) s t
    (Branch B va vb vc vd);

paint :: forall a b. Color -> Rbta a b -> Rbta a b;
paint c Empty = Empty;
paint c (Branch uu l k v r) = Branch c l k v r;

balance_right :: forall a b. Rbta a b -> a -> b -> Rbta a b -> Rbta a b;
balance_right a k x (Branch R b s y c) = Branch R a k x (Branch B b s y c);
balance_right (Branch B a k x b) s y Empty =
  balance (Branch R a k x b) s y Empty;
balance_right (Branch B a k x b) s y (Branch B va vb vc vd) =
  balance (Branch R a k x b) s y (Branch B va vb vc vd);
balance_right (Branch R a k x (Branch B b s y c)) t z Empty =
  Branch R (balance (paint R a) k x b) s y (Branch B c t z Empty);
balance_right (Branch R a k x (Branch B b s y c)) t z (Branch B va vb vc vd) =
  Branch R (balance (paint R a) k x b) s y
    (Branch B c t z (Branch B va vb vc vd));
balance_right Empty k x Empty = Empty;
balance_right (Branch R va vb vc Empty) k x Empty = Empty;
balance_right (Branch R va vb vc (Branch R ve vf vg vh)) k x Empty = Empty;
balance_right Empty k x (Branch B va vb vc vd) = Empty;
balance_right (Branch R ve vf vg Empty) k x (Branch B va vb vc vd) = Empty;
balance_right (Branch R ve vf vg (Branch R vi vj vk vl)) k x
  (Branch B va vb vc vd) = Empty;

balance_left :: forall a b. Rbta a b -> a -> b -> Rbta a b -> Rbta a b;
balance_left (Branch R a k x b) s y c = Branch R (Branch B a k x b) s y c;
balance_left Empty k x (Branch B a s y b) =
  balance Empty k x (Branch R a s y b);
balance_left (Branch B va vb vc vd) k x (Branch B a s y b) =
  balance (Branch B va vb vc vd) k x (Branch R a s y b);
balance_left Empty k x (Branch R (Branch B a s y b) t z c) =
  Branch R (Branch B Empty k x a) s y (balance b t z (paint R c));
balance_left (Branch B va vb vc vd) k x (Branch R (Branch B a s y b) t z c) =
  Branch R (Branch B (Branch B va vb vc vd) k x a) s y
    (balance b t z (paint R c));
balance_left Empty k x Empty = Empty;
balance_left Empty k x (Branch R Empty vb vc vd) = Empty;
balance_left Empty k x (Branch R (Branch R ve vf vg vh) vb vc vd) = Empty;
balance_left (Branch B va vb vc vd) k x Empty = Empty;
balance_left (Branch B va vb vc vd) k x (Branch R Empty vf vg vh) = Empty;
balance_left (Branch B va vb vc vd) k x
  (Branch R (Branch R vi vj vk vl) vf vg vh) = Empty;

combine :: forall a b. Rbta a b -> Rbta a b -> Rbta a b;
combine Empty x = x;
combine (Branch v va vb vc vd) Empty = Branch v va vb vc vd;
combine (Branch R a k x b) (Branch R c s y d) =
  (case combine b c of {
    Empty -> Branch R a k x (Branch R Empty s y d);
    Branch R b2 t z c2 -> Branch R (Branch R a k x b2) t z (Branch R c2 s y d);
    Branch B b2 t z c2 -> Branch R a k x (Branch R (Branch B b2 t z c2) s y d);
  });
combine (Branch B a k x b) (Branch B c s y d) =
  (case combine b c of {
    Empty -> balance_left a k x (Branch B Empty s y d);
    Branch R b2 t z c2 -> Branch R (Branch B a k x b2) t z (Branch B c2 s y d);
    Branch B b2 t z c2 ->
      balance_left a k x (Branch B (Branch B b2 t z c2) s y d);
  });
combine (Branch B va vb vc vd) (Branch R b k x c) =
  Branch R (combine (Branch B va vb vc vd) b) k x c;
combine (Branch R a k x b) (Branch B va vb vc vd) =
  Branch R a k x (combine b (Branch B va vb vc vd));

rbt_del :: forall a b. (Prelude.Ord a) => a -> Rbta a b -> Rbta a b;
rbt_del x Empty = Empty;
rbt_del x (Branch c a y s b) =
  (if x < y then rbt_del_from_left x a y s b
    else (if y < x then rbt_del_from_right x a y s b else combine a b));

rbt_del_from_left ::
  forall a b.
    (Prelude.Ord a) => a -> Rbta a b -> a -> b -> Rbta a b -> Rbta a b;
rbt_del_from_left x (Branch B lt z v rt) y s b =
  balance_left (rbt_del x (Branch B lt z v rt)) y s b;
rbt_del_from_left x Empty y s b = Branch R (rbt_del x Empty) y s b;
rbt_del_from_left x (Branch R va vb vc vd) y s b =
  Branch R (rbt_del x (Branch R va vb vc vd)) y s b;

rbt_del_from_right ::
  forall a b.
    (Prelude.Ord a) => a -> Rbta a b -> a -> b -> Rbta a b -> Rbta a b;
rbt_del_from_right x a y s (Branch B lt z v rt) =
  balance_right a y s (rbt_del x (Branch B lt z v rt));
rbt_del_from_right x a y s Empty = Branch R a y s (rbt_del x Empty);
rbt_del_from_right x a y s (Branch R va vb vc vd) =
  Branch R a y s (rbt_del x (Branch R va vb vc vd));

rbt_delete :: forall a b. (Prelude.Ord a) => a -> Rbta a b -> Rbta a b;
rbt_delete k t = paint B (rbt_del k t);

impl_of :: forall b a. (Prelude.Ord b) => Rbt b a -> Rbta b a;
impl_of (RBT x) = x;

delete :: forall a b. (Prelude.Ord a) => a -> Rbt a b -> Rbt a b;
delete xb xc = RBT (rbt_delete xb (impl_of xc));

rbt_ins ::
  forall a b.
    (Prelude.Ord a) => (a -> b -> b -> b) -> a -> b -> Rbta a b -> Rbta a b;
rbt_ins f k v Empty = Branch R Empty k v Empty;
rbt_ins f k v (Branch B l x y r) =
  (if k < x then balance (rbt_ins f k v l) x y r
    else (if x < k then balance l x y (rbt_ins f k v r)
           else Branch B l x (f k y v) r));
rbt_ins f k v (Branch R l x y r) =
  (if k < x then Branch R (rbt_ins f k v l) x y r
    else (if x < k then Branch R l x y (rbt_ins f k v r)
           else Branch R l x (f k y v) r));

rbt_insert_with_key ::
  forall a b.
    (Prelude.Ord a) => (a -> b -> b -> b) -> a -> b -> Rbta a b -> Rbta a b;
rbt_insert_with_key f k v t = paint B (rbt_ins f k v t);

rbt_insert :: forall a b. (Prelude.Ord a) => a -> b -> Rbta a b -> Rbta a b;
rbt_insert = rbt_insert_with_key (\ _ _ nv -> nv);

insert :: forall a b. (Prelude.Ord a) => a -> b -> Rbt a b -> Rbt a b;
insert xc xd xe = RBT (rbt_insert xc xd (impl_of xe));

rbt_lookup :: forall a b. (Prelude.Ord a) => Rbta a b -> a -> Maybe b;
rbt_lookup Empty k = Nothing;
rbt_lookup (Branch uu l x y r) k =
  (if k < x then rbt_lookup l k
    else (if x < k then rbt_lookup r k else Just y));

lookup :: forall a b. (Prelude.Ord a) => Rbt a b -> a -> Maybe b;
lookup x = rbt_lookup (impl_of x);

rbt_height :: (Integer -> Integer -> Integer) -> Rbta a b -> Integer;
rbt_height _ Empty = 0;
rbt_height f (Branch _ l _ _ r)  = 1 + f (rbt_height f l) (rbt_height f r);

rbt_height_balenced :: Rbta a b -> Bool; 
rbt_height_balenced t = rbt_height Prelude.max t <= 2 * rbt_height Prelude.min t;

height_balanced :: Prelude.Ord a => Rbt a b -> Bool;
height_balanced = rbt_height_balenced . impl_of;

height :: Prelude.Ord a => (Integer -> Integer -> Integer) -> Rbt a b -> Integer;
height f = rbt_height f . impl_of;

}
