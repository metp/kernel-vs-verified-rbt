module Strategy (Cmd, Result(..), random, exhaustive) where

import Control.Monad
import Data.Bifunctor
import Data.List
import RBT.Kernel (IRBT, Cmd(..))
import RBT.Verified (Tree, Color)
import System.Random.Shuffle (shuffle')
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

vCmd :: IRBT -> (Cmd,Int) -> IRBT
vCmd t (c,x) = (fst $ cmdMap c) x t

kCmd :: Kernel.Handle -> (Cmd,Int) -> IO IRBT
kCmd hdl (c,x) = (snd $ cmdMap c) hdl x

buildInput :: [Int] -> [Int] -> [Cmd] -> [(Cmd, Int)]
buildInput _ _ [] = []
buildInput (i:is) ds (Insert : cs) = (Insert, i) : buildInput is ds cs
buildInput is (d:ds) (Delete : cs) = (Delete, d) : buildInput is ds cs

type TestStrategy = Int -> Int -> IO [[Result]]

random :: TestStrategy
random runs seed = do
  let rndCmds = randoms seed :: [Cmd]
  let rndXs = nub $ map abs $ randoms seed :: [Int]
  let inputs = take runs (buildInput rndXs rndXs rndCmds)
  let vs = tail $ scanl vCmd RBT.empty inputs
  hdl <- Kernel.init
  ks <- mapM (kCmd hdl) inputs
  Kernel.cleanup hdl
  let (cs,xs) = unzip inputs
  return $ zipWith5 Result cs xs vs ks (RBT.empty : ks) : []
  where
    randoms :: Uniform a => Int -> [a]
    randoms = unfoldr (Just . uniform) . mkStdGen

exhaustive :: TestStrategy
exhaustive n seed = do
  let allDistributions = [replicate i Insert ++ replicate (n-i) Delete | i <- [0..n]]
  let shuffle1ToN = shuffle' [1..n] n . mkStdGen
  let insShuffled = shuffle1ToN seed
  let delShuffled = shuffle1ToN (succ seed)
  let inputLists = concatMap (permutations . buildInput insShuffled delShuffled) allDistributions :: [[(Cmd,Int)]]
  let verifiedTrees = map (tail . scanl vCmd RBT.empty) inputLists :: [[IRBT]]
  hdl <- Kernel.init
  kernelTrees <- mapM (\is -> Kernel.reset hdl >> mapM (kCmd hdl) is) inputLists :: IO [[IRBT]]
  Kernel.cleanup hdl
  return $ zipWith3 (\is vs ks -> let (cs, xs) = unzip is in zipWith5 Result cs xs vs ks (RBT.empty : ks)) inputLists verifiedTrees kernelTrees
