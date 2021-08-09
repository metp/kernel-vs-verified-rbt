module Strategy (Result(..), random, exhaustive, symbolic) where

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
  input :: Input,
  vTree :: IRBT,
  kTreeIO :: IO IRBT }

instance Uniform Cmd where
  uniformM g = toEnum <$> uniformRM (succ minCmd, maxCmd) g
    where
      minCmd = fromEnum (minBound :: Cmd)
      maxCmd = fromEnum (maxBound :: Cmd)

cmdMap :: Cmd -> (Word64 -> IRBT -> IRBT, Kernel.Handle -> Word64 -> IO IRBT)
cmdMap Insert = (Verified.insert, Kernel.insert)
cmdMap Delete = (Verified.delete, Kernel.delete)
cmdMap Reset = undefined

vCmd :: IRBT -> Input -> IRBT
vCmd t (c,x) = (fst $ cmdMap c) x t

kCmd :: Kernel.Handle -> Input -> IO IRBT
kCmd hdl (c,x) = (snd $ cmdMap c) hdl x

buildInput :: [Word64] -> [Word64] -> [Cmd] -> TestCase Input
buildInput _ _ [] = []
buildInput (i:is) ds (Insert : cs) = (Insert, i) : buildInput is ds cs
buildInput is (d:ds) (Delete : cs) = (Delete, d) : buildInput is ds cs
buildInput is ds (Reset : cs) = buildInput is ds cs
buildInput _ _ _ = undefined

buildResults :: Kernel.Handle -> [TestCase Input] -> [TestCase Result]
buildResults hdl testCases = do
  inputs <- testCases
  let vTrees = tail $ scanl vCmd RBT.empty inputs
  let kTrees = map (kCmd hdl) inputs
  return $ zipWith3 Result inputs vTrees kTrees

random :: Kernel.Handle -> Word64 -> Int -> [TestCase Result]
random hdl runs seed = do
  let rndCmds = randoms seed
  let rndXs = randoms seed
  let inputs = genericTake runs (buildInput rndXs rndXs rndCmds)
  buildResults hdl [inputs]
  where
    randoms :: Uniform a => Int -> [a]
    randoms = unfoldr (Just . uniform) . mkStdGen

exhaustive :: Kernel.Handle -> Word64 -> [TestCase Result]
exhaustive hdl n = do
  let distributions = [genericReplicate i Insert ++ genericReplicate (n-i) Delete | i <- [n,n-1..]]
  let inputRuns = concatMap (permutations . buildInput [1..n] [1..n]) distributions
  buildResults hdl inputRuns

symbolic :: Kernel.Handle -> FilePath -> IO (Either String [TestCase Result])
symbolic hdl = fmap (buildResults hdl <$>) . parseStdins
