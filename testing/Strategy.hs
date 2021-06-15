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
  kTree :: IO IRBT }

instance Uniform Cmd where
  uniformM g = toEnum <$> uniformRM (succ minCmd, maxCmd) g
    where
      minCmd = fromEnum (minBound :: Cmd)
      maxCmd = fromEnum (maxBound :: Cmd)

cmdMap :: Cmd -> (Int -> IRBT -> IRBT, Kernel.Handle -> Int -> IO IRBT)
cmdMap Insert = (Verified.insert, Kernel.insert)
cmdMap Delete = (Verified.delete, Kernel.delete)
cmdMap Reset = undefined

vCmd :: IRBT -> (Cmd,Int) -> IRBT
vCmd t (c,x) = (fst $ cmdMap c) x t

kCmd :: Kernel.Handle -> (Cmd,Int) -> IO IRBT
kCmd hdl (c,x) = (snd $ cmdMap c) hdl x

buildInput :: [Int] -> [Int] -> [Cmd] -> [(Cmd, Int)]
buildInput _ _ [] = []
buildInput (i:is) ds (Insert : cs) = (Insert, i) : buildInput is ds cs
buildInput is (d:ds) (Delete : cs) = (Delete, d) : buildInput is ds cs
buildInput _ _ _ = undefined

type TestStrategy = Kernel.Handle -> Int -> Int -> [[Result]]

random :: TestStrategy
random hdl runs seed = do
  let rndCmds = randoms seed :: [Cmd]
  let rndXs = nub $ map abs $ randoms seed :: [Int]
  let inputs = take runs (buildInput rndXs rndXs rndCmds)
  let vs = tail $ scanl vCmd RBT.empty inputs
  let ks = map (kCmd hdl) inputs
  let (cs,xs) = unzip inputs
  [zipWith4 Result cs xs vs ks]
  where
    randoms :: Uniform a => Int -> [a]
    randoms = unfoldr (Just . uniform) . mkStdGen

exhaustive :: TestStrategy
exhaustive hdl n seed = do
  let allDistributions = [replicate i Insert ++ replicate (n-i) Delete | i <- [0..n]]
  let shuffle1ToN = shuffle' [1..n] n . mkStdGen
  let insShuffled = shuffle1ToN seed
  let delShuffled = shuffle1ToN (succ seed)
  let inputLists = concatMap (permutations . buildInput insShuffled delShuffled) allDistributions :: [[(Cmd,Int)]]
  let verifiedTrees = map (tail . scanl vCmd RBT.empty) inputLists :: [[IRBT]]
  let kernelTrees = map (map (kCmd hdl)) inputLists :: [[IO IRBT]]
  zipWith3 (\is vs ks -> let (cs, xs) = unzip is in zipWith4 Result cs xs vs ks) inputLists verifiedTrees kernelTrees
