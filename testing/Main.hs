{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}

module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Options.Applicative
import RBT.Verified (Tree, Color, rbt, inorder, invc, invh, rootBlack)
import System.Random
import System.Random.Shuffle
import qualified RBT.Kernel as Kernel
import qualified RBT.Verified as RBT (empty)
import qualified RBT.Verified as Verified (insert, delete)

type IRBT = Tree (Int, Color)

data Options = Options
  { runs :: Int
  , seed :: Int
  , verbose :: Bool }

naturalParser :: ReadM Int
naturalParser = eitherReader $ \s -> if read s >= 0
  then Right $ read s
  else Left "Not a positive value"

options :: ParserInfo Options
options = info (opts <**> helper) desc where
  desc = fullDesc <> header "Userspace testing harness for the Linux red-black tree implementation"
  opts = do
    verbose    <- switch $ short 'v' <> help "verbose"
    runs       <- option naturalParser $ short 'n' <> metavar "<runs>" <> help "Number of runs"
    seed       <- option auto $ short 's' <> metavar "<seed>" <> showDefault <> value 42
                  <> help "Seed for the pseudo-random-number generator"
    pure Options {..}

main :: IO ()
main = do
  opts <- execParser options
  let gen = mkStdGen $ seed opts
  let range = tokenRange $ runs opts
  let ns = take (runs opts) $ nub $ unfoldr (Just . uniformR range) gen
  let ns' = shuffle' ns (runs opts) gen
  let v = verbose opts
  (hdls,kTreeInit) <- Kernel.init
  insTrees <- foldM (exec v "inserting" Verified.insert (Kernel.insert hdls)) (RBT.empty, kTreeInit) ns
  foldM_ (exec v "deleting" Verified.delete (Kernel.delete hdls)) insTrees ns'
  Kernel.cleanup hdls

exec :: Bool -> String -> (Int -> IRBT -> IRBT) -> (Int -> IO IRBT) -> (IRBT,IRBT) -> Int -> IO (IRBT,IRBT)
exec verbose op opVerified opKernel (vTreePrev,kTreePrev) k = do
  let vTree = opVerified k vTreePrev
  kTree <- opKernel k
  when verbose $ print kTree
  unless (rbt kTree && inorder kTree == inorder vTree) $ do
    let invs = [
          ("color"     ,  invc kTree),
          ("height"    ,  invh kTree),
          ("root_black",  rootBlack kTree),
          ("inorder"   ,  inorder vTree == inorder kTree)
          ]
    putStr $ "After " ++ op ++ " " ++ show k ++ " following invariants failed: "
    putStrLn $ unwords $ map fst $ filter (not . snd) invs
    putStrLn $ "Previously: " ++ show kTreePrev
    putStrLn $ "Now:        " ++ show kTree
    putStrLn $ "Verified:   " ++ show vTree
    putStrLn ""
  return (vTree,kTree)

tokenRange :: Int -> (Int,Int)
tokenRange n | n < 10 = (0,9)
tokenRange n = (10^(d - 1), 10^d - 1)
  where d = fromJust $ elemIndex 0 (iterate (`div` 10) n)
