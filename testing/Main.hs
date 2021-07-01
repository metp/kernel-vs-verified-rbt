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
  , randomTest :: Bool
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
    randomTest <- switch $ short 'r' <> help "Use the random test strategy instead of exhaustive testing one"
    runs       <- option naturalParser $ short 'n' <> metavar "<runs>" <> help "Number of runs"
    seed       <- option auto $ short 's' <> metavar "<seed>" <> showDefault <> value 42
                  <> help "Seed for the pseudo-random-number generator"
    pure Options {..}

restartHeader = "------Restart------\n"

main :: IO ()
main = do
  Options{..} <- execParser options
  hdl <- Kernel.init
  let rss = (if randomTest
      then Strategy.random
      else Strategy.exhaustive) hdl runs seed
  forM_ rss $ \rs -> do
    Kernel.reset hdl
    when verbose $ putStrLn restartHeader
    checkResults verbose RBT.empty rs
  Kernel.cleanup hdl

compareTrees :: IRBT -> IRBT -> Either [String] ()
compareTrees vTree kTree
  | rbt kTree && inorder kTree == inorder vTree = Right ()
  | otherwise = Left $ map fst $ filter (not . snd) [
      ("color"     ,  invc kTree) ,
      ("height"    ,  invh kTree) ,
      ("root_black",  rootBlack kTree) ,
      ("inorder"   ,  inorder vTree == inorder kTree) ]

printTrees :: Cmd -> Int -> IRBT -> IRBT -> IRBT -> [String] -> IO ()
printTrees cmd key vTree kTree kTreePrev invs = do
  putStrLn $ unwords $ if null invs
  then [show cmd, show key]
  else ["After", show cmd, show key, "following invariants failed:"] ++ invs
  putStrLn $ "Kernel tree before:  " ++ show kTreePrev
  putStrLn $ "Kernel tree after:   " ++ show kTree
  putStrLn $ "Verified tree after: " ++ show vTree
  putStrLn ""

checkResults :: Bool -> IRBT -> [Result] -> IO ()
checkResults _ _ [] = return ()
checkResults verbose kTreePrev (Result{..}:rs) = do
  kTree' <- kTree
  case compareTrees vTree kTree' of
    Left invs ->
      printTrees cmd key vTree kTree' kTreePrev invs
    Right _ -> do 
      when verbose $ printTrees cmd key vTree kTree' kTreePrev []
      checkResults verbose kTree' rs
