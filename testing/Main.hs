module Main where

import Control.Monad
import Data.Bifunctor
import Data.List
import Data.Maybe
import Options.Applicative
import RBT.Kernel (IRBT)
import RBT.Verified
import Strategy
import System.Random
import System.Random.Shuffle
import qualified RBT.Kernel as Kernel
import qualified RBT.Verified as RBT (empty)
import qualified RBT.Verified as Verified (insert, delete)

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
  Options{..} <- execParser options
  rs <- Strategy.random runs seed
  if verbose
  then mapM_ (printState []) rs
  else do
    let status = mapM checkResult rs
    either (uncurry printState) (pure () `const`) status

checkResult :: Result -> Either ([String], Result) ()
checkResult r@Result{..}
  | rbt kTree && inorder kTree == inorder vTree = Right ()
  | otherwise = do
      let failed = map fst $ filter (not . snd) invs
      Left (failed, r)
      where invs = ("color"     ,  invc kTree) :
                   ("height"    ,  invh kTree) :
                   ("root_black",  rootBlack kTree) :
                   ("inorder"   ,  inorder vTree == inorder kTree) : []

printState :: [String] -> Result -> IO ()
printState invs Result{..} = do
  putStrLn $ unwords $ if null invs
  then [show cmd, show key]
  else ["After", show cmd, show key, "following invariants failed:"] ++ invs
  putStrLn $ "Kernel tree before:  " ++ show kTreePrev
  putStrLn $ "Kernel tree after:   " ++ show kTree
  putStrLn $ "Verified tree after: " ++ show vTree
  putStrLn ""
