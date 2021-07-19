module Strategy (Input, Run, Cmd, Result(..), random, exhaustive, symbolic) where

import Control.Monad
import Data.Bifunctor
import Data.List
import Data.Word (Word64)
import InputCollection
import RBT.Kernel (IRBT, Cmd(..))
import RBT.Verified (Tree, Color)
import System.Random (uniform, mkStdGen)
import System.Random.Stateful (Uniform, uniformM, uniformRM)
import qualified RBT.Kernel as Kernel
import qualified RBT.Verified as RBT (empty)
import qualified RBT.Verified as Verified (insert, delete)
import Control.Applicative

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

vCmd :: IRBT -> (Cmd,Word64) -> IRBT
vCmd t (c,x) = (fst $ cmdMap c) x t

kCmd :: Kernel.Handle -> (Cmd,Word64) -> IO IRBT
kCmd hdl (c,x) = (snd $ cmdMap c) hdl x

buildInput :: [Word64] -> [Word64] -> [Cmd] -> Run Input
buildInput _ _ [] = []
buildInput (i:is) ds (Insert : cs) = (Insert, i) : buildInput is ds cs
buildInput is (d:ds) (Delete : cs) = (Delete, d) : buildInput is ds cs
buildInput _ _ _ = undefined

buildResult :: Kernel.Handle -> [Run Input] -> [Run Result]
buildResult hdl inputs = do
  let verifiedTrees = map (tail . scanl vCmd RBT.empty) inputs
  let kernelTrees = map (map (kCmd hdl)) inputs
  zipWith3 (uncurry (zipWith4 Result) . unzip) inputs verifiedTrees kernelTrees

random :: Kernel.Handle -> Word64 -> Int -> [Run Result]
random hdl runs seed = do
  let rndCmds = randoms seed
  let rndXs = randoms seed
  let inputs = genericTake runs (buildInput rndXs rndXs rndCmds)
  buildResult hdl [inputs]
  where
    randoms :: Uniform a => Int -> [a]
    randoms = unfoldr (Just . uniform) . mkStdGen

exhaustive :: Kernel.Handle -> Word64 -> [Run Result]
exhaustive hdl n = do
  let distributions = [genericReplicate i Insert ++ genericReplicate (n-i) Delete | i <- [n,n-1..]]
  let inputRuns = concatMap (permutations . buildInput [1..n] [1..n]) distributions
  buildResult hdl inputRuns

symbolic :: Kernel.Handle -> FilePath -> IO (Either String [Run Result])
symbolic hdl = fmap (buildResult hdl <$>) . parseStdins
