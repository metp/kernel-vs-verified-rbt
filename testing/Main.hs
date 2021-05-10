module Main where

import RBT.Verified (Rbt, empty, height_balanced, height)
import qualified RBT.Verified as Verified (insert, delete)
import qualified RBT.Kernel as Kernel (insert, delete, reset)
import Control.Monad (when, unless)
import System.Environment (getArgs)

main :: IO ()
main = do
  Kernel.reset
  n <- read . head <$> getArgs
  testLoop [1..n] empty

type IRbt = Rbt Integer Integer

rbtLoop :: IRbt -> IO ()
rbtLoop tree = do
  cmd <- getLine

  when (cmd == "insert") $ do
    key <- read <$> getLine
    value <- read <$> getLine
    let newTree = Verified.insert key value tree
    print newTree
    rbtLoop newTree

  when (cmd == "delete") $ do
    key <- read <$> getLine
    let newTree = Verified.delete key tree
    print newTree
    rbtLoop newTree

testLoop :: [Integer] -> IRbt -> IO ()
testLoop [] _ = return ()
testLoop (k:ks) tree = do
  let verifiedTree = Verified.insert k 0 tree
  kernelTree <- Kernel.insert k 0
--  when (show verifiedTree /= show kernelTree) $ do
--  unless (height_balanced kernelTree) $ do
  when True $ do
    putStrLn $ "Trees after inserting from 1 to " ++ show k
    putStrLn $ "Verified: " ++ show verifiedTree
    putStrLn $ "Kernel:   " ++ show kernelTree
    putStrLn $ "Diff:     " ++ diff (show verifiedTree) (show kernelTree)
    putStrLn $ "Verified: height_balanced = " ++ show (height_balanced verifiedTree) ++ " " ++ show (height max verifiedTree) ++ " " ++ show (height min verifiedTree)
    putStrLn $ "Kernel:   height_balanced = " ++ show (height_balanced kernelTree) ++ " " ++ show (height max kernelTree) ++ " " ++ show (height min kernelTree)
    putStrLn ""

  testLoop ks verifiedTree

diff :: String -> String -> String
diff as bs = concatMap (\(a,b) -> if a == b then "." else "X" ) (zip as bs) ++ concat (replicate (abs $ length as - length bs) ".")
