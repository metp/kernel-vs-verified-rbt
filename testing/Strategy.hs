module Strategy (Cmd, Result(..), random, randoms) where

import Control.Monad
import Data.Bifunctor
import Data.List
import RBT.Kernel (IRBT, Cmd(..))
import RBT.Verified (Tree, Color)
import System.Random (uniform, mkStdGen)
import System.Random.Stateful (Uniform, uniformM, uniformRM)
import qualified RBT.Kernel as Kernel
import qualified RBT.Verified as RBT (empty)
import qualified RBT.Verified as Verified (insert, delete)

data Result = Result {
  cmd :: Cmd,
  key :: Int,
  vTree :: IRBT,
  kTree :: IRBT ,
  kTreePrev :: IRBT }

instance Uniform Cmd where
  uniformM g = toEnum <$> uniformRM (succ minCmd, maxCmd) g
    where
      minCmd = fromEnum (minBound :: Cmd)
      maxCmd = fromEnum (maxBound :: Cmd)

cmdMap :: Cmd -> (Int -> IRBT -> IRBT, Kernel.Handle -> Int -> IO IRBT)
cmdMap Insert = (Verified.insert, Kernel.insert)
cmdMap Delete = (Verified.delete, Kernel.delete)

vCmd = fst . cmdMap
kCmd = snd . cmdMap

randoms :: Uniform a => Int -> [a]
randoms = unfoldr (Just . uniform) . mkStdGen

random :: Int -> Int -> IO [Result]
random runs seed = do
  hdl <- Kernel.init
  ksNow <- ks hdl
  let ksPrev = RBT.empty : ksNow
  let (cs,xs) = unzip is
  let rs = zipWith5 Result cs xs vs ksNow ksPrev
  Kernel.cleanup hdl
  return rs
  where
    is = take runs $ randomInput seed
    vs = tail $ scanl (\t (c,x) -> vCmd c x t) RBT.empty is
    ks hdl = mapM (\(c,x) -> kCmd c hdl x) is

randomInput :: Int -> [(Cmd, Int)]
randomInput seed = randomInput' cs xs xs where
  randomInput' [] _ _ = []
  randomInput' (Insert : cs) (i:is) ds = (Insert, i) : randomInput' cs is ds
  randomInput' (Delete : cs) is (d:ds) = (Delete, d) : randomInput' cs is ds
  cs = randoms seed :: [Cmd]
  xs = nub $ map abs $ randoms seed :: [Int]
