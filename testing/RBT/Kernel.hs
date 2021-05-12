module RBT.Kernel where

import RBT.Verified (Tree, Color)

keyFile = "/sys/kernel/debug/rbt_if/key"
cmdFile = "/sys/kernel/debug/rbt_if/cmd"

insert :: Int -> IO (Tree (Int,Color))
insert k = do
  writeFile keyFile $ show k
  writeFile cmdFile "0"
  read <$> readFile cmdFile

delete :: Int -> IO (Tree (Int,Color))
delete k = do
  writeFile keyFile $ show k
  writeFile cmdFile "1"
  read <$> readFile cmdFile

reset :: IO ()
reset = writeFile cmdFile "2"
