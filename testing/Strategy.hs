module Strategy (Cmd, Result(..), random, exhaustive) where

import Control.Monad
import Data.Bifunctor
import Data.List
import Data.Word (Word64)
import RBT.Kernel (IRBT, Cmd(..))
import RBT.Verified (Tree, Color)
import System.Random (uniform, mkStdGen)
import System.Random.Shuffle (shuffle')
import System.Random.Stateful (Uniform, uniformM, uniformRM)
import qualified RBT.Kernel as Kernel
import qualified RBT.Verified as RBT (empty)
import qualified RBT.Verified as Verified (insert, delete)

data Result = Result {
  cmd :: Cmd,
  key :: Word64,
  vTree :: IRBT,
  kTree :: IO IRBT }

instance Uniform Cmd where
  uniformM g = toEnum <$> uniformRM (succ minCmd, maxCmd) g
    where
      minCmd = fromEnum (minBound :: Cmd)
      maxCmd = fromEnum (maxBound :: Cmd)

cmdMap :: Cmd -> (Word64 -> IRBT -> IRBT, Kernel.Handle -> Word64 -> IO IRBT)
cmdMap Insert = (Verified.insert, Kernel.insert)
cmdMap Delete = (Verified.delete, Kernel.delete)
cmdMap Reset = undefined

type Input = (Cmd,Word64)
type Run a = [a]

vCmd :: IRBT -> (Cmd,Word64) -> IRBT
vCmd t (c,x) = (fst $ cmdMap c) x t

kCmd :: Kernel.Handle -> (Cmd,Word64) -> IO IRBT
kCmd hdl (c,x) = (snd $ cmdMap c) hdl x

buildInput :: [Word64] -> [Word64] -> [Cmd] -> Run Input
buildInput _ _ [] = []
buildInput (i:is) ds (Insert : cs) = (Insert, i) : buildInput is ds cs
buildInput is (d:ds) (Delete : cs) = (Delete, d) : buildInput is ds cs
buildInput _ _ _ = undefined

type TestStrategy = Kernel.Handle -> Word64 -> Int -> [Run Result]

random :: TestStrategy
random hdl runs seed = do
  let rndCmds = randoms seed :: [Cmd]
  let rndXs = nub $ randoms seed :: [Word64]
  let inputs = genericTake runs (buildInput rndXs rndXs rndCmds)
  let vs = tail $ scanl vCmd RBT.empty inputs
  let ks = map (kCmd hdl) inputs
  let (cs,xs) = unzip inputs
  [zipWith4 Result cs xs vs ks]
  where
    randoms :: Uniform a => Int -> [a]
    randoms = unfoldr (Just . uniform) . mkStdGen

exhaustive :: TestStrategy
exhaustive hdl n _ = do
  let distributions = [genericReplicate i Insert ++ genericReplicate (n-i) Delete | i <- [n,n-1..]]
  let inputLists = concatMap (permutations . buildInput [1..n] [1..n]) distributions :: [Run Input]
  let verifiedTrees = map (tail . scanl vCmd RBT.empty) inputLists :: [Run IRBT]
  let kernelTrees = map (map (kCmd hdl)) inputLists :: [Run (IO IRBT)]
  zipWith3 (\is vs ks -> let (cs, xs) = unzip is in zipWith4 Result cs xs vs ks) inputLists verifiedTrees kernelTrees
