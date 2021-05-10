module RBT.Kernel where

import RBT.Verified (Rbt)

keyFile = "/sys/kernel/debug/rbt_if/key"
valueFile = "/sys/kernel/debug/rbt_if/value"
cmdFile = "/sys/kernel/debug/rbt_if/cmd"

insert :: Integer -> Integer -> IO (Rbt Integer Integer)
insert k v = do
  writeFile keyFile $ show k
  writeFile valueFile $ show v
  writeFile cmdFile "0"
  read <$> readFile cmdFile

delete :: Integer -> IO (Rbt Integer Integer)
delete k = do
  writeFile keyFile $ show k
  writeFile cmdFile "1"
  read <$> readFile cmdFile


reset :: IO ()
reset = writeFile cmdFile "2"
