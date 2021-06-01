module RBT.Kernel(RBT.Kernel.init, cleanup, insert, delete) where

import RBT.Verified (Tree, Color)
import System.IO
import GHC.IO.Handle

keyFile = "/sys/kernel/debug/rbt_if/key"
cmdFile = "/sys/kernel/debug/rbt_if/cmd"

data Cmd = Reset | Insert | Replace | Delete deriving (Enum)

instance Show Cmd where
  show = show . fromEnum

data KernelHandle = KernelHandle { keyHdl :: Handle, cmdHdl :: Handle }

init :: IO (KernelHandle, Tree (Int, Color))
init = do
  keyHdl <- openFile keyFile WriteMode
  cmdHdl <- openFile cmdFile ReadWriteMode
  hSetBuffering keyHdl LineBuffering
  hSetBuffering cmdHdl LineBuffering
  hPrint cmdHdl Reset
  leaf <- read <$> hGetLine cmdHdl
  return (KernelHandle keyHdl cmdHdl, leaf)

cleanup :: KernelHandle -> IO ()
cleanup hdls = do
  hClose $ keyHdl hdls
  hClose $ cmdHdl hdls

insert :: KernelHandle -> Int -> IO (Tree (Int,Color))
insert (KernelHandle keyHdl cmdHdl) k = do
  hPrint keyHdl k
  hPrint cmdHdl Insert
  hSeek cmdHdl AbsoluteSeek 0
  read <$> hGetLine cmdHdl

delete :: KernelHandle -> Int -> IO (Tree (Int,Color))
delete (KernelHandle keyHdl cmdHdl) k = do
  hPrint keyHdl k
  hPrint cmdHdl Delete
  hSeek cmdHdl AbsoluteSeek 0
  read <$> hGetLine cmdHdl
