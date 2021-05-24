{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}

module Main where

import RBT.Verified (Tree, Color)
import qualified RBT.Verified as RBT (empty)
import qualified RBT.Verified as Verified (insert, delete)
import qualified RBT.Kernel as Kernel (insert, delete, reset)
import Control.Monad (when, unless, foldM_)
import System.Environment (getArgs)
import System.Random
import Data.List
import Data.Maybe
import Options.Applicative

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
  Kernel.reset
  let gen = mkStdGen $ seed opts
  let range = tokenRange $ runs opts
  let numbers = take (runs opts) $ nub $ unfoldr (Just . uniformR range) gen
  foldM_ (test $ verbose opts)  RBT.empty numbers

test :: Bool -> Tree (Int, Color) -> Int -> IO (Tree (Int, Color))
test verbose tree k = do
  let verifiedTree = Verified.insert k tree
  kernelTree <- Kernel.insert k
  when (verbose || show verifiedTree /= show kernelTree) $ do
    putStrLn $ "Trees after inserting " ++ show k
    putStrLn $ "Verified: " ++ show verifiedTree
    putStrLn $ "Kernel:   " ++ show kernelTree
    putStrLn $ "Diff:     " ++ diff (show verifiedTree) (show kernelTree)
    putStrLn ""
  return verifiedTree

diff :: String -> String -> String
diff as bs = [if a == b then '.' else 'X' | (a,b) <- zip as bs] ++ replicate (abs $ length as - length bs) '.'

tokenRange :: Int -> (Int,Int)
tokenRange n | n < 10 = (0,9)
tokenRange n = (10^(d - 1), 10^d - 1)
  where d = fromJust $ elemIndex 0 (iterate (`div` 10) n)
